(ns datamigo.storage
  "Per-namespace (per-pubkey) persistence for the sync server. Each namespace is an
   aztree keyed timestamp -> {entity {attribute value}}, mirroring the
   client's :ordered structure so the shared negentropy protocol reconciles
   them directly.

   In-memory {pubkey-hex -> aztree} is the serving truth, reconstructed from LMDB
   on demand (get-or-load-tree). LMDB is a durable write-behind mirror: a single
   writer thread drains a diff mailbox and persists, so no request thread ever
   blocks on a write and write transactions stay on one stable OS thread (LMDB
   requires thread-stable write txns). A crash loses at most the un-drained tail of
   the mailbox, which the next negentropy sync re-reconciles."
  (:require [aztree :as aztree]
            [clojure.edn :as edn])
  (:import (java.util.concurrent Executors ExecutorService)
           (java.util.concurrent.locks ReentrantLock)
           (java.nio ByteBuffer)
           (java.io File)
           (org.lmdbjava Env EnvFlags DbiFlags KeyRange PutFlags)))

;; ── LMDB ──────────────────────────────────────────────────────────────────────
;;
;; Key layout, all keys prefixed by the 32 raw pubkey bytes of the namespace:
;;   [pubkey 32B][(pr-str timestamp) utf-8]  ->  (pr-str {entity attmap})
;;
;; The pubkey prefix keeps each namespace's records contiguous, so loading a
;; namespace is a single prefix range scan.

(defonce ^Env env
  (let [dir (File. "data")]
    (.mkdirs dir)
    (-> (Env/create)
        (.setMapSize (* 10 1024 1024 1024))
        (.setMaxDbs 1)
        (.open dir (make-array EnvFlags 0)))))

(defonce db (.openDbi env "data" (into-array DbiFlags [DbiFlags/MDB_CREATE])))

(defn- ^ByteBuffer record-key [^bytes pubkey-bytes ordered-key]
  (let [suffix (.getBytes (pr-str ordered-key) "UTF-8")
        buf (ByteBuffer/allocateDirect (+ 32 (alength suffix)))]
    (.put buf pubkey-bytes) (.put buf suffix) (.flip buf) buf))

(defn- ^ByteBuffer namespace-prefix [^bytes pubkey-bytes]
  (let [buf (ByteBuffer/allocateDirect 32)]
    (.put buf pubkey-bytes) (.flip buf) buf))

(defn- record-key->ordered-key
  "Read the timestamp key back from a stored record key, skipping the
   32-byte pubkey prefix."
  [^ByteBuffer key]
  (let [suffix (byte-array (- (.remaining key) 32))
        dup (doto (.duplicate key) (.position 32))]
    (.get dup suffix)
    (edn/read-string (String. suffix "UTF-8"))))

(defn- ^ByteBuffer value->buf [value]
  (let [b (.getBytes (pr-str value) "UTF-8")
        buf (ByteBuffer/allocateDirect (alength b))]
    (.put buf b) (.flip buf) buf))

(defn- buf->value [^ByteBuffer buf]
  (let [b (byte-array (.remaining buf))]
    (.get buf b)
    (edn/read-string (String. b "UTF-8"))))

(defn- pubkey-prefix? [^ByteBuffer key ^bytes pubkey-bytes]
  (and (>= (.remaining key) 32)
       (let [arr (byte-array 32)]
         (.get (.duplicate key) arr)
         (java.util.Arrays/equals arr pubkey-bytes))))

;; ── Loading a namespace from LMDB ─────────────────────────────────────────────

(defn load-tree
  "Reconstruct the aztree for a namespace by scanning its LMDB records. Runs a read
   transaction on the calling thread (LMDB MVCC lets it proceed alongside the
   writer)."
  [^bytes pubkey-bytes]
  (with-open [txn (.txnRead env)
              cursor (.iterate db txn (KeyRange/atLeast (namespace-prefix pubkey-bytes)))]
    (let [iter (.iterator cursor)]
      (loop [tree (aztree/atree)]
        (if (.hasNext iter)
          (let [kv (.next iter)
                k (.key kv)]
            (if (pubkey-prefix? k pubkey-bytes)
              (let [ordered-key (record-key->ordered-key k)
                    entity-map (buf->value (.val kv))]
                (recur (assoc tree ordered-key entity-map)))
              tree))
          tree)))))

;; ── In-memory namespace cache ─────────────────────────────────────────────────
;;
;; {pubkey-hex -> aztree}, unbounded. A namespace is written only by its owner and
;; that owner syncs sequentially, so there is exactly one mutator per namespace and
;; no tree-level contention. Loads are guarded per-key so two concurrent first
;; accesses don't both scan LMDB.

(defonce ^:private trees (atom {}))
(defonce ^:private load-lock (ReentrantLock.))

(defn get-or-load-tree
  "Return the in-memory aztree for pubkey-hex, reconstructing it from LMDB on the
   first access. The single access point for namespace trees, so a bounded cache
   can later replace the unbounded map here without touching callers."
  [pubkey-hex ^bytes pubkey-bytes]
  (or (get @trees pubkey-hex)
      (do
        (.lock load-lock)
        (try
          (or (get @trees pubkey-hex)
              (let [tree (load-tree pubkey-bytes)]
                (swap! trees assoc pubkey-hex tree)
                tree))
          (finally (.unlock load-lock))))))

(defn patch-tree!
  "Apply a negentropy diff {:puts [[key value]...] :deletes [key...]} to the
   namespace's in-memory tree, replacing it. Called on the sync (push) path by the
   owner's request thread; safe without locking because only the owner mutates
   their namespace and does so sequentially."
  [pubkey-hex {:keys [puts deletes]}]
  (let [tree (get @trees pubkey-hex)
        tree (reduce (fn [t k] (dissoc t k)) tree deletes)
        tree (reduce (fn [t [k v]] (assoc t k v)) tree puts)]
    (swap! trees assoc pubkey-hex tree)
    tree))

;; ── Writer thread ─────────────────────────────────────────────────────────────
;;
;; A single platform thread persists diffs to LMDB. Request threads enqueue and
;; return immediately (fire-and-forget); the writer keeps every write txn on this
;; one stable thread. Enqueued jobs are [pubkey-bytes diff].

(defonce ^ExecutorService writer (Executors/newSingleThreadExecutor))

(defn- persist-diff! [^bytes pubkey-bytes {:keys [puts deletes]}]
  (let [txn (.txnWrite env)]
    (try
      (doseq [ordered-key deletes]
        (.delete db txn (record-key pubkey-bytes ordered-key)))
      (doseq [[ordered-key entity-map] puts]
        (.put db txn (record-key pubkey-bytes ordered-key) (value->buf entity-map) (make-array PutFlags 0)))
      (.commit txn)
      (catch Exception e
        (.abort txn)
        (println (str "STORAGE WRITE FAILED for namespace; will re-reconcile next sync. error: " e))))))

(defn enqueue-diff!
  "Fire-and-forget: hand a diff to the writer thread to persist. Returns immediately;
   a lost diff (crash before drain) is re-reconciled on the next sync."
  [^bytes pubkey-bytes diff]
  (.submit writer ^Runnable (fn [] (persist-diff! pubkey-bytes diff)))
  nil)
