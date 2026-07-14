(ns datamigo.core
  (:require [aleph.http :as http]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [clojure.edn :as edn])
  (:import (fr.acinq.secp256k1 Secp256k1)
           (java.security MessageDigest)
           (java.util HexFormat Arrays)
           (java.nio ByteBuffer)
           (java.io File)
           (org.lmdbjava Env EnvFlags DbiFlags KeyRange PutFlags)))

;; ── Crypto ──────────────────────────────────────────────────────────────────

(defonce ^Secp256k1 secp256k1 (Secp256k1/get))
(defonce hex-format (HexFormat/of))

(defn hex-string->bytes ^bytes [^String s] (.parseHex hex-format s))
(defn hex-bytes->string ^String [^bytes b] (.formatHex hex-format b))

(defn string->sha256-bytes ^bytes [^String s]
  (-> (MessageDigest/getInstance "SHA-256") (.digest (.getBytes s "UTF-8"))))

(defn verify-schnorr-signature [^bytes pubkey ^bytes msg ^bytes sig]
  (try (.verifySchnorr secp256k1 sig msg pubkey) (catch Exception _ false)))

;; ── LMDB ────────────────────────────────────────────────────────────────────
;;
;; Key layout within a single DBI, all keys prefixed by 32 raw pubkey bytes:
;;
;;   [pubkey 32B][0x00]["permitted"]                   → pr-str(#{pubkey-hex ...})
;;   [pubkey 32B][0x01][entity-utf8][0x00][attr-utf8]  → pr-str([hash-hex value])
;;
;; The type byte (0x00 vs 0x01) keeps permission entries sorted before data
;; entries within each pubkey's namespace, so a permission check is a direct
;; get rather than a scan.

(defonce env
  (let [dir (File. "data")]
    (.mkdirs dir)
    (-> (Env/create)
        (.setMapSize (* 10 1024 1024 1024))
        (.setMaxDbs 1)
        (.open dir (make-array EnvFlags 0)))))

(defonce db (.openDbi env "data" (into-array DbiFlags [DbiFlags/MDB_CREATE])))

(defn ^ByteBuffer str->buf [^String s]
  (let [b (.getBytes s "UTF-8") buf (ByteBuffer/allocateDirect (alength b))]
    (.put buf b) (.flip buf) buf))

(defn ^String buf->str [^ByteBuffer buf]
  (let [b (byte-array (.remaining buf))] (.get buf b) (String. b "UTF-8")))

(defn ^ByteBuffer permission-key [^bytes pb]
  (let [p (.getBytes "permitted" "UTF-8")
        buf (ByteBuffer/allocateDirect (+ 32 1 (alength p)))]
    (.put buf pb) (.put buf (byte 0x00)) (.put buf p) (.flip buf) buf))

(defn ^ByteBuffer data-key [^bytes pb entity attr]
  (let [eb (.getBytes (pr-str entity) "UTF-8")
        ab (.getBytes (pr-str attr)   "UTF-8")
        buf (ByteBuffer/allocateDirect (+ 32 1 (alength eb) 1 (alength ab)))]
    (.put buf pb) (.put buf (byte 0x01))
    (.put buf eb) (.put buf (byte 0x00)) (.put buf ab)
    (.flip buf) buf))

(defn ^ByteBuffer data-prefix [^bytes pb]
  (let [buf (ByteBuffer/allocateDirect 33)]
    (.put buf pb) (.put buf (byte 0x01)) (.flip buf) buf))

(defn pubkey-prefix? [^ByteBuffer key ^bytes pb]
  (and (>= (.remaining key) 32)
       (let [arr (byte-array 32)]
         (.get (.duplicate key) arr)
         (Arrays/equals arr pb))))

(defn parse-data-key [^ByteBuffer key]
  ;; skip pubkey(32) + type(1), split remainder on the 0x00 separator, returns [entity attribute]
  (let [dup       (doto (.duplicate key) (.position 33))
        e0a       (byte-array (.remaining dup))
        _         (.get dup e0a)
        sep       (loop [i 0]
                    (if (or (= i (alength e0a)) (zero? (aget e0a i))) i (recur (inc i))))
        entity    (edn/read-string (String. e0a 0 sep "UTF-8"))
        attribute (edn/read-string (String. e0a (inc sep) (- (alength e0a) sep 1) "UTF-8"))]
    [entity attribute]))

(defn triple-hash ^bytes [entity attr val]
  (let [he (string->sha256-bytes (pr-str entity))
        ha (string->sha256-bytes (pr-str attr))
        hv (string->sha256-bytes (pr-str val))]
    (byte-array (mapv bit-xor he ha hv))))

(defn read-permission [pubkey-hex]
  (with-open [txn (.txnRead env)]
    (when-let [v (.get db txn (permission-key (hex-string->bytes pubkey-hex)))]
      (edn/read-string (buf->str v)))))

(defn write-permission [pubkey-hex permitted-set]
  (with-open [txn (.txnWrite env)]
    (.put db txn (permission-key (hex-string->bytes pubkey-hex)) (str->buf (pr-str permitted-set)) (make-array PutFlags 0))
    (.commit txn)))

(defn read-namespace [pubkey-hex]
  (let [pb (hex-string->bytes pubkey-hex)]
    (with-open [txn (.txnRead env)
                ci  (.iterate db txn (KeyRange/atLeast (data-prefix pb)))]
      (let [iter (.iterator ci)]
        (loop [result {}]
          (if (.hasNext iter)
            (let [kv (.next iter) k (.key kv)]
              (if (pubkey-prefix? k pb)
                (let [[entity attr] (parse-data-key k)
                      [_hash val]   (edn/read-string (buf->str (.val kv)))]
                  (recur (assoc-in result [entity attr] val)))
                result))
            result))))))

(defn scan-buckets
  "Scans [start, end), divides items into n buckets, returns a vector of
   {:start :end :count :fp :items} descriptors."
  [pubkey-hex start end n threshold]
  (let [pb        (hex-string->bytes pubkey-hex)
        start-key (if start (data-key pb (first start) (second start)) (data-prefix pb))
        key-range (if end
                    (KeyRange/closedOpen start-key (data-key pb (first end) (second end)))
                    (KeyRange/atLeast start-key))]
    (with-open [txn (.txnRead env)
                ci  (.iterate db txn key-range)]
      (let [iter      (.iterator ci)
            all-items (loop [items []]
                        (if (.hasNext iter)
                          (let [kv (.next iter) k (.key kv)]
                            (if (pubkey-prefix? k pb)
                              (let [[entity attr] (parse-data-key k)
                                    [hash-hex _]  (edn/read-string (buf->str (.val kv)))]
                                (recur (conj items [entity attr hash-hex])))
                              items))
                          items))
            total     (count all-items)]
        (if (zero? total)
          [{:start start :end end :count 0 :fp (hex-bytes->string (byte-array 32)) :items []}]
          (let [k       (min n total)
                bsize   (int (Math/ceil (/ (double total) k)))
                pvec    (vec (partition-all bsize all-items))
                nbuckets (count pvec)]
            (mapv (fn [i]
                    (let [bucket       (pvec i)
                          bucket-start (if (zero? i) start (let [[e a _] (first bucket)] [e a]))
                          bucket-end   (if (< (inc i) nbuckets)
                                         (let [[e a _] (first (pvec (inc i)))] [e a])
                                         end)
                          fp           (reduce (fn [acc [_ _ h]]
                                                 (byte-array (mapv bit-xor acc (hex-string->bytes h))))
                                               (byte-array 32)
                                               bucket)]
                      {:start bucket-start
                       :end   bucket-end
                       :count (count bucket)
                       :fp    (hex-bytes->string fp)
                       :items (vec bucket)}))
                  (range nbuckets))))))))

(defn apply-diff! [pubkey-hex diff]
  (let [pb      (hex-string->bytes pubkey-hex)
        add     (get diff 'add {})
        retract (get diff 'retract {})]
    (with-open [txn (.txnWrite env)]
      (doseq [[entity attmap] retract [attr _] attmap]
        (.delete db txn (data-key pb entity attr)))
      (doseq [[entity attmap] add [attr val] attmap]
        (.put db txn (data-key pb entity attr)
              (str->buf (pr-str [(hex-bytes->string (triple-hash entity attr val)) val]))
              (make-array PutFlags 0)))
      (.commit txn))))

;; ── State ────────────────────────────────────────────────────────────────────

(def streams (atom {:unauthenticated {:stream->nonce {}}
                    :authenticated   {:stream->pubkey {} :pubkey->stream {}}}))

(def watchers (atom {})) ; {pubkey-hex -> #{stream}}

(def pubkey->active-sync-id (atom {}))

;; ── Operations ───────────────────────────────────────────────────────────────

(defn send-and-close! [stream msg]
  (-> (s/put! stream (pr-str msg)) (d/chain (fn [_] (s/close! stream)))))

(def ^:const bucket-count 16)
(def ^:const item-threshold 4)

(defn do-sync [stream sync-id pubkey & ranges]
  (if-let [caller (get-in @streams [:authenticated :stream->pubkey stream])]
    (if (= caller pubkey)
      (do
        (swap! pubkey->active-sync-id assoc pubkey sync-id)
        (let [sub-ranges  (mapcat (fn [[start end fp]]
                                    (let [server-fp (:fp (first (scan-buckets pubkey start end 1 0)))]
                                      (if (= server-fp fp)
                                        []
                                        (scan-buckets pubkey start end bucket-count item-threshold))))
                                  ranges)
              items-mode? (and (seq sub-ranges) (<= (:count (first sub-ranges)) item-threshold))]
          (if (empty? sub-ranges)
            (s/put! stream (pr-str ['end-sync [sync-id pubkey {'add {} 'retract {}}]]))
            (s/put! stream (pr-str ['sync (into [sync-id pubkey]
                                                (mapv (fn [{:keys [start end fp items]}]
                                                        (if items-mode? [start end items] [start end fp]))
                                                      sub-ranges))])))))
      (send-and-close! stream "not authorized"))
    (send-and-close! stream "not authenticated")))

(defn do-end-sync [stream sync-id pubkey diff]
  (if-let [caller (get-in @streams [:authenticated :stream->pubkey stream])]
    (if (= caller pubkey)
      (when (= (get @pubkey->active-sync-id pubkey) sync-id)
        (apply-diff! pubkey diff)
        (swap! pubkey->active-sync-id dissoc pubkey)
        (let [msg (pr-str ['end-sync [sync-id pubkey diff]])]
          (doseq [ws (get @watchers pubkey #{})] (s/put! ws msg))))
      (send-and-close! stream "not authorized"))
    (send-and-close! stream "not authenticated")))

(defn do-pull [stream target-pubkeys]
  (if-let [caller (get-in @streams [:authenticated :stream->pubkey stream])]
    (let [result (reduce (fn [acc target]
                           (if (contains? (read-permission target) caller)
                             (do (swap! watchers update target (fnil conj #{}) stream)
                                 (assoc acc target (read-namespace target)))
                             acc))
                         {}
                         target-pubkeys)]
      (s/put! stream (pr-str ['pull result])))
    (send-and-close! stream "not authenticated")))

(defn do-permit [stream permitted-set]
  (if-let [pubkey (get-in @streams [:authenticated :stream->pubkey stream])]
    (write-permission pubkey permitted-set)
    (send-and-close! stream "not authenticated")))

;; ── WebSocket handlers ────────────────────────────────────────────────────────

(defn authenticate-stream! [stream pubkey]
  (swap! streams (fn [s] (-> s
                              (update-in [:unauthenticated :stream->nonce] dissoc stream)
                              (assoc-in  [:authenticated   :stream->pubkey stream] pubkey)
                              (assoc-in  [:authenticated   :pubkey->stream pubkey] stream)))))

(defn on-receive [stream message]
  (if-let [msg (try (edn/read-string message) (catch Exception _ nil))]
    (let [op (try (first msg) (catch Exception _ (s/put! stream (pr-str "malformed message")) msg))]
      (case op
        liar?
        (try (let [{:keys [pubkey signature]} (second msg)
                   nonce (get-in @streams [:unauthenticated :stream->nonce stream])]
               (if (and nonce pubkey signature
                        (verify-schnorr-signature (hex-string->bytes pubkey)
                                                  (string->sha256-bytes nonce)
                                                  (hex-string->bytes signature)))
                 (authenticate-stream! stream pubkey)
                 (send-and-close! stream "not authenticated")))
             (catch Exception _ (s/put! stream (pr-str "malformed liar? response"))))
        sync     (apply do-sync     stream (second msg))
        end-sync (apply do-end-sync stream (second msg))
        pull     (do-pull           stream (second msg))
        permit   (do-permit         stream (second msg))
        (send-and-close! stream "not authenticated")))
    (send-and-close! stream "not authenticated")))

(defn on-open [stream]
  (let [nonce (str (java.util.UUID/randomUUID))]
    (swap! streams assoc-in [:unauthenticated :stream->nonce stream] nonce)
    (s/put! stream (pr-str ['liar? nonce]))))

(defn on-close [stream]
  (swap! watchers (fn [w]
                    (reduce-kv (fn [acc k v]
                                 (let [v' (disj v stream)]
                                   (if (empty? v') acc (assoc acc k v'))))
                               {} w)))
  (if-let [pubkey (get-in @streams [:authenticated :stream->pubkey stream])]
    (do (swap! pubkey->active-sync-id dissoc pubkey)
        (swap! streams (fn [s] (-> s
                                    (update-in [:authenticated :stream->pubkey] dissoc stream)
                                    (update-in [:authenticated :pubkey->stream] dissoc pubkey)))))
    (swap! streams update-in [:unauthenticated :stream->nonce] dissoc stream)))

(defn handler [request]
  (-> (d/let-flow [stream (http/websocket-connection request)]
        (on-open stream)
        (s/consume #(on-receive stream %) stream)
        (s/on-closed stream #(on-close stream)))
      (d/catch (fn [_]
                 {:status 200 :headers {"content-type" "text/html"} :body "not connecting via a websocket"}))))

(defn start-server! []
  (http/start-server handler {:port 8080}))

(defn -main [& args]
  (println "Starting datamigo...")
  (start-server!))
