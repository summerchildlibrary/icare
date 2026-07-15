(ns datamigo.http
  "REST sync server. Replaces the old WebSocket protocol with client-driven
   negentropy over HTTP, plus long-poll watch/poke for change notification.

   Concurrency: a fixed platform-thread pool serves requests (single-core target,
   so threads simply block on slow I/O and the OS schedules around them). All LMDB
   writes go through datamigo.storage's single writer thread; reads (cold-tree
   loads, sync rounds against the immutable in-memory trees) run inline on the
   request thread.

   Roles: the client always initiates and drives the HTTP rounds; the server is the
   responder. Each POST /sync is one negentropy round. On the PUSH path (owner
   writing its own namespace) the client finishes by POSTing the diff to /commit,
   which the server applies to memory, enqueues to LMDB, and pokes watchers on. On
   the PULL path (reading a friend's namespace) the client applies the diff locally
   and never commits."
  (:require [datamigo.storage :as storage]
            [icare.negentropy :as negentropy]
            [clojure.edn :as edn])
  (:import (com.sun.net.httpserver HttpServer HttpHandler HttpExchange)
           (fr.acinq.secp256k1 Secp256k1)
           (java.security MessageDigest)
           (java.net InetSocketAddress)
           (java.util HexFormat UUID)
           (java.util.concurrent Executors LinkedBlockingQueue ConcurrentHashMap TimeUnit)
           (java.io ByteArrayOutputStream)))

(def hex-format (HexFormat/of))
(defn- hex->bytes [^String s] (.parseHex hex-format s))

;; ── Crypto ──────────────────────────────────────────────────────────────────

(defonce ^Secp256k1 secp256k1 (Secp256k1/get))

(defn- ^bytes sha256 [^String s]
  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))

(defn- verify-schnorr [^bytes pubkey ^bytes msg ^bytes sig]
  (try (.verifySchnorr secp256k1 sig msg pubkey) (catch Exception _ false)))

;; ── Commit tokens (replay protection) ─────────────────────────────────────────
;;
;; The db is open to reads; only commits are authenticated, proving the caller
;; owns the namespace (holds its private key). To stop replay without depending on
;; client clocks, the server issues a fresh token on every /sync response and keeps
;; only the latest per namespace. A commit must carry that token, signed together
;; with the diff; using it removes it, and issuing a new one supersedes the old, so
;; a captured commit can never be replayed. One entry per namespace, no expiry.

(defonce ^ConcurrentHashMap commit-tokens (ConcurrentHashMap.))

(defn- issue-token! [namespace-hex]
  (let [token (str (UUID/randomUUID))]
    (.put commit-tokens namespace-hex token)
    token))

(defn- consume-token! [namespace-hex token]
  ;; atomic: removes only if `token` is the current one for the namespace
  (and token (.remove commit-tokens namespace-hex token)))

;; ── Watchers ──────────────────────────────────────────────────────────────────
;;
;; {namespace-pubkey-hex -> #{queue}} where each queue is a watcher's mailbox. A
;; watcher long-polls by parking on its queue for up to a minute; a commit to a
;; namespace offers that namespace's hex to every registered watcher, waking them
;; to go sync.

(defonce ^:private watchers (atom {}))

(defn- register-watcher! [namespaces queue]
  (swap! watchers (fn [w] (reduce (fn [w ns] (update w ns (fnil conj #{}) queue)) w namespaces))))

(defn- unregister-watcher! [namespaces queue]
  (swap! watchers (fn [w] (reduce (fn [w ns]
                                    (let [q' (disj (get w ns #{}) queue)]
                                      (if (empty? q') (dissoc w ns) (assoc w ns q'))))
                                  w namespaces))))

(defn- poke-watchers! [namespace-hex]
  (doseq [^LinkedBlockingQueue q (get @watchers namespace-hex)]
    (.offer q namespace-hex)))

;; ── HTTP helpers ──────────────────────────────────────────────────────────────

(defn- read-body [^HttpExchange exchange]
  (with-open [in (.getRequestBody exchange)
              out (ByteArrayOutputStream.)]
    (.transferTo in out)
    (edn/read-string (.toString out "UTF-8"))))

(defn- respond! [^HttpExchange exchange status body]
  (let [bytes (.getBytes (pr-str body) "UTF-8")]
    (.sendResponseHeaders exchange status (alength bytes))
    (with-open [out (.getResponseBody exchange)]
      (.write out bytes))))

(defn- handler [f]
  (reify HttpHandler
    (handle [_ exchange]
      (try
        (f exchange)
        (catch Exception e
          (println (str "handler error: " e))
          (try (respond! exchange 500 {:error (str e)}) (catch Exception _ nil)))
        (finally (.close exchange))))))

;; ── Endpoints ─────────────────────────────────────────────────────────────────

(defn- handle-sync
  "One negentropy round. Body: {:namespace hex :ranges [[low high fp]...]}. The
   server is the responder: it loads the namespace's tree and returns the
   sub-ranges describing where it differs from the client's fingerprints, plus a
   fresh :commit-token the owner will sign into a subsequent commit. Reads are open,
   so no auth here; the token only matters if this client goes on to commit."
  [^HttpExchange exchange]
  (let [{:keys [namespace ranges]} (read-body exchange)
        tree (storage/get-or-load-tree namespace (hex->bytes namespace))
        sub-ranges (negentropy/respond tree ranges)]
    (respond! exchange 200 {:sub-ranges sub-ranges
                            :commit-token (issue-token! namespace)})))

(defn- handle-commit
  "Push path, authenticated. Body: {:namespace hex :diff {:puts :deletes}
   :commit-token t :signature hex}. The signature is a Schnorr sig by the
   namespace's key over sha256(pr-str [namespace diff commit-token]) — proving key
   ownership and binding this exact diff and token. The token must be the current
   one for the namespace (consumed on use, superseded by later syncs), which stops
   replay. On success: patch memory, enqueue the LMDB write, poke watchers."
  [^HttpExchange exchange]
  (let [{:keys [namespace diff commit-token signature]} (read-body exchange)
        pubkey-bytes (hex->bytes namespace)
        message (sha256 (pr-str [namespace diff commit-token]))]
    (cond
      (not (consume-token! namespace commit-token))
      (respond! exchange 403 {:error "invalid or stale commit token"})

      (not (and signature (verify-schnorr pubkey-bytes message (hex->bytes signature))))
      (respond! exchange 403 {:error "invalid signature"})

      :else
      (do
        (storage/patch-tree! namespace diff)
        (storage/enqueue-diff! pubkey-bytes diff)
        (poke-watchers! namespace)
        (respond! exchange 200 {:ok true})))))

(def ^:private watch-timeout-ms (* 60 1000))

(defn- handle-watch
  "Long-poll. Body: {:namespaces [friend-hex...]}. Park up to a minute; return
   {:poked ns-hex} when any watched namespace commits, or {:poked nil} on timeout.
   The client re-issues to keep watching."
  [^HttpExchange exchange]
  (let [{:keys [namespaces]} (read-body exchange)
        queue (LinkedBlockingQueue.)]
    (register-watcher! namespaces queue)
    (try
      (let [poked (.poll queue watch-timeout-ms TimeUnit/MILLISECONDS)]
        (respond! exchange 200 {:poked poked}))
      (finally (unregister-watcher! namespaces queue)))))

;; ── Server ────────────────────────────────────────────────────────────────────

(defonce ^:private server (atom nil))

(defn start-server!
  ([] (start-server! 8080 16))
  ([port pool-size]
   (let [srv (HttpServer/create (InetSocketAddress. port) 0)]
     (.setExecutor srv (Executors/newFixedThreadPool pool-size))
     (.createContext srv "/sync" (handler handle-sync))
     (.createContext srv "/commit" (handler handle-commit))
     (.createContext srv "/watch" (handler handle-watch))
     (.start srv)
     (reset! server srv)
     (println (str "datamigo listening on :" port))
     srv)))

(defn stop-server! []
  (when-let [srv @server]
    (.stop srv 0)
    (reset! server nil)))
