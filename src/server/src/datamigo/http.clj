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
            [datamigo.push :as push]
            [negentropy :as negentropy]
            [clojure.edn :as edn])
  (:import (com.sun.net.httpserver HttpServer HttpHandler HttpExchange)
           (fr.acinq.secp256k1 Secp256k1)
           (java.security MessageDigest)
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

(defn handler [f]
  (reify HttpHandler
    (handle [_ exchange]
      (try
        (f exchange)
        (catch Exception e
          (println (str "handler error: " e))
          (try (respond! exchange 500 {:error (str e)}) (catch Exception _ nil)))
        (finally (.close exchange))))))

;; ── Endpoints ─────────────────────────────────────────────────────────────────

(defn handle-sync
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

(defn handle-commit
  "Push path, authenticated. Body: {:namespace hex :diff {:puts :deletes}
   :commit-token t :signature hex}. The signature is a Schnorr sig by the
   namespace's key over sha256(pr-str [namespace diff commit-token]) — proving key
   ownership and binding this exact diff and token. The token must be the current
   one for the namespace (consumed on use, superseded by later syncs), which stops
   replay. On success: patch memory, enqueue the LMDB write, poke watchers."
  [^HttpExchange exchange]
  (let [{:keys [namespace diff-edn commit-token signature]} (read-body exchange)
        pubkey-bytes (hex->bytes namespace)
        ;; Verify against the exact string the client signed and sent. Re-serializing
        ;; a parsed diff is NOT byte-stable — Clojure promotes maps larger than 8
        ;; entries from array-map (insertion order) to hash-map (hash order), so the
        ;; re-print came back reordered and every such signature failed.
        message (sha256 (str namespace "|" diff-edn "|" commit-token))
        diff (edn/read-string diff-edn)]
    (println "COMMIT attempt from namespace" namespace
             "puts" (count (:puts diff)) "deletes" (count (:deletes diff)))
    (cond
      (not (consume-token! namespace commit-token))
      (do (println "  -> REJECTED: invalid or stale commit token")
          (respond! exchange 403 {:error "invalid or stale commit token"}))

      (not (and signature (verify-schnorr pubkey-bytes message (hex->bytes signature))))
      (do (println "  -> REJECTED: invalid signature")
          (respond! exchange 403 {:error "invalid signature"}))

      :else
      ;; completions are detected against the tree as it stands BEFORE patching:
      ;; the prior versions live under exactly the keys patch-tree! is about to
      ;; delete, so this must not move below it
      (let [pushes (push/completion-pushes
                    namespace
                    (storage/get-or-load-tree namespace pubkey-bytes)
                    diff)]
        (storage/patch-tree! namespace diff)
        (storage/enqueue-diff! pubkey-bytes diff)
        (poke-watchers! namespace)
        (push/dispatch! pushes)
        (respond! exchange 200 {:ok true})))))

(defn handle-ping
  "Plain GET so a phone browser can prove it can reach this server on the LAN."
  [^HttpExchange exchange]
  (let [bytes (.getBytes "datamigo ok\n" "UTF-8")]
    (.sendResponseHeaders exchange 200 (alength bytes))
    (with-open [out (.getResponseBody exchange)]
      (.write out bytes))))

(def ^:private watch-timeout-ms (* 60 1000))

(defn handle-register
  "Record push interests and return immediately. Body: {:device-token t
   :interests {namespace [entity...]}}.

   /watch carries the same payload, but it parks for a minute, so interests
   created by a fresh like would not reach the server until the current poll
   expired — and would be lost entirely if the app were backgrounded first. This
   is the eager path; /watch keeps sending them as a self-healing refresh."
  [^HttpExchange exchange]
  (let [{:keys [device-token interests]} (read-body exchange)]
    (push/register-interests! device-token interests)
    (respond! exchange 200 {:ok true})))

(defn handle-watch
  "Long-poll. Body: {:namespaces [friend-hex...]} plus, optionally,
   :device-token and :interests {namespace [entity...]} to receive pushes while
   not polling. Park up to a minute; return {:poked ns-hex} when any watched
   namespace commits, or {:poked nil} on timeout. The client re-issues to keep
   watching, which also refreshes its push interests."
  [^HttpExchange exchange]
  (let [{:keys [namespaces device-token interests]} (read-body exchange)
        queue (LinkedBlockingQueue.)]
    ;; the watcher queue is per-poll; push interest outlives it, which is the
    ;; whole point — it is how you get told about a commit while disconnected
    (push/register-interests! device-token interests)
    (register-watcher! namespaces queue)
    (try
      (let [poked (.poll queue watch-timeout-ms TimeUnit/MILLISECONDS)]
        (respond! exchange 200 {:poked poked}))
      (finally (unregister-watcher! namespaces queue)))))
