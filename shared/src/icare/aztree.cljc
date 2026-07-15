(ns icare.aztree
  "An augmented zip-zip tree: a persistent, history-independent binary search tree
   where every node caches the XOR-folded fingerprint aggregate and the entry count
   of its subtree, so range-fingerprint, range-size and split-range run in O(log n).
   Purpose-built for negentropy set reconciliation, where two endpoints localize
   their differences by exchanging XOR-folded fingerprints over successively
   narrower key ranges. Shared verbatim between the ClojureDart client and the JVM
   servers so both sides fingerprint and reconcile identically."
  #?(:cljd (:require ["dart:convert" :as convert])))

;; ── 64-bit wrapping arithmetic ───────────────────────────────────────────────
;; Murmur3 relies on multiply/add wrapping modulo 2^64. ClojureDart ints are
;; 64-bit two's-complement and already wrap; on the JVM plain * / + throw on
;; overflow, so we use the unchecked variants. These helpers localize the only
;; real platform difference in the hash.

(defn- wrap* [a b] #?(:cljd (* a b) :clj (unchecked-multiply a b)))
(defn- wrap+ [a b] #?(:cljd (+ a b) :clj (unchecked-add a b)))

(defn- utf8-bytes [s]
  #?(:cljd (convert/utf8.encode s)
     :clj  (.getBytes ^String s "UTF-8")))

(defn- byte-length [bytes]
  #?(:cljd (.-length bytes) :clj (alength ^bytes bytes)))

(defn- rotate-left-64 [value rotation]
  (bit-or (bit-shift-left value rotation)
          (unsigned-bit-shift-right value (- 64 rotation))))

(defn- finalization-mix-64 [value]
  (let [value (bit-xor value (unsigned-bit-shift-right value 33))
        value (wrap* value -49064778989728563)
        value (bit-xor value (unsigned-bit-shift-right value 33))
        value (wrap* value -4265267296055464877)
        value (bit-xor value (unsigned-bit-shift-right value 33))]
    value))

(def ^:private murmur-constant-1 -8663945395140668459)
(def ^:private murmur-constant-2 5545529020109919103)

(defn fingerprint
  "Murmur3 x64 128 of the UTF-8 bytes of (pr-str value), returned as two 64-bit
   words [high low] and clamped away from all-zero (which is the XOR identity and
   would make an entry invisible in the aggregate). The single indirection every
   caller uses to fingerprint a value; changing hash functions happens here."
  [value]
  (let [string (pr-str value)
        bytes (utf8-bytes string)
        length (byte-length bytes)
        block-count (quot length 16)]
    (loop [block-index 0 high 0 low 0]
      (if (< block-index block-count)
        (let [block-base (* block-index 16)
              block-high (loop [byte-index 0 accumulator 0]
                           (if (< byte-index 8)
                             (recur (inc byte-index)
                                    (bit-or accumulator
                                            (bit-shift-left
                                             (bit-and (aget bytes (+ block-base byte-index)) 0xFF)
                                             (* byte-index 8))))
                             accumulator))
              block-low (loop [byte-index 0 accumulator 0]
                          (if (< byte-index 8)
                            (recur (inc byte-index)
                                   (bit-or accumulator
                                           (bit-shift-left
                                            (bit-and (aget bytes (+ block-base 8 byte-index)) 0xFF)
                                            (* byte-index 8))))
                            accumulator))
              block-high (wrap* (rotate-left-64 (wrap* block-high murmur-constant-1) 31) murmur-constant-2)
              high (bit-xor high block-high)
              high (wrap* (wrap+ (rotate-left-64 high 27) low) 5)
              high (wrap+ high 1390208809)
              block-low (wrap* (rotate-left-64 (wrap* block-low murmur-constant-2) 33) murmur-constant-1)
              low (bit-xor low block-low)
              low (wrap* (wrap+ (rotate-left-64 low 31) high) 5)
              low (wrap+ low 944331445)]
          (recur (inc block-index) high low))
        (let [tail-base (* block-count 16)
              tail-length (- length tail-base)
              tail-high (loop [byte-index 0 accumulator 0]
                          (if (and (< byte-index 8) (< (+ tail-base byte-index) length))
                            (recur (inc byte-index)
                                   (bit-or accumulator
                                           (bit-shift-left
                                            (bit-and (aget bytes (+ tail-base byte-index)) 0xFF)
                                            (* byte-index 8))))
                            accumulator))
              tail-low (loop [byte-index 8 accumulator 0]
                         (if (and (< byte-index 16) (< (+ tail-base byte-index) length))
                           (recur (inc byte-index)
                                  (bit-or accumulator
                                          (bit-shift-left
                                           (bit-and (aget bytes (+ tail-base byte-index)) 0xFF)
                                           (* (- byte-index 8) 8))))
                           accumulator))
              high (if (pos? tail-length)
                     (bit-xor high (wrap* (rotate-left-64 (wrap* tail-high murmur-constant-1) 31) murmur-constant-2))
                     high)
              low (if (> tail-length 8)
                    (bit-xor low (wrap* (rotate-left-64 (wrap* tail-low murmur-constant-2) 33) murmur-constant-1))
                    low)
              high (bit-xor high length)
              low (bit-xor low length)
              high (wrap+ high low)
              low (wrap+ low high)
              high (finalization-mix-64 high)
              low (finalization-mix-64 low)
              high (wrap+ high low)
              low (wrap+ low high)]
          (if (and (zero? high) (zero? low))
            [1 0]
            [high low]))))))

(defn- trailing-zeros-64 [value]
  (loop [value value trailing-zeros 0]
    (if (or (= trailing-zeros 64) (odd? value))
      trailing-zeros
      (recur (unsigned-bit-shift-right value 1) (inc trailing-zeros)))))

;; ── Node ─────────────────────────────────────────────────────────────────────

(deftype AZNode
  [entry-key
   entry-value
   fingerprint-high
   fingerprint-low
   aggregate-high
   aggregate-low
   subtree-count
   left
   right]
  #?@(:cljd [:type-only true]))

(defn- node-count [node]
  (if (nil? node) 0 (.-subtree-count ^AZNode node)))

(defn- node-aggregate-high [node]
  (if (nil? node) 0 (.-aggregate-high ^AZNode node)))

(defn- node-aggregate-low [node]
  (if (nil? node) 0 (.-aggregate-low ^AZNode node)))

(defn- make-node
  [entry-key entry-value fingerprint-high fingerprint-low left right]
  (AZNode. entry-key entry-value fingerprint-high fingerprint-low
    (bit-xor (node-aggregate-high left) fingerprint-high (node-aggregate-high right))
    (bit-xor (node-aggregate-low left) fingerprint-low (node-aggregate-low right))
    (+ 1 (node-count left) (node-count right))
    left right))

(defn- outranks?
  [candidate-fingerprint-high candidate-fingerprint-low candidate-key
   incumbent-fingerprint-high incumbent-fingerprint-low incumbent-key
   compare-keys]
  (let [candidate-rank (trailing-zeros-64 candidate-fingerprint-high)
        incumbent-rank (trailing-zeros-64 incumbent-fingerprint-high)]
    (cond
      (not= candidate-rank incumbent-rank)
      (> candidate-rank incumbent-rank)

      (not= candidate-fingerprint-low incumbent-fingerprint-low)
      (> candidate-fingerprint-low incumbent-fingerprint-low)

      :else
      (neg? (compare-keys candidate-key incumbent-key)))))

(defn- unzip
  [node split-key compare-keys]
  (if (nil? node)
    [nil nil]
    (let [n ^AZNode node
          comparison (compare-keys split-key (.-entry-key n))]
      (if (neg? comparison)
        (let [[less greater] (unzip (.-left n) split-key compare-keys)]
          [less (make-node (.-entry-key n) (.-entry-value n)
                           (.-fingerprint-high n) (.-fingerprint-low n)
                           greater (.-right n))])
        (let [[less greater] (unzip (.-right n) split-key compare-keys)]
          [(make-node (.-entry-key n) (.-entry-value n)
                      (.-fingerprint-high n) (.-fingerprint-low n)
                      (.-left n) less)
           greater])))))

(defn- zip
  [less greater compare-keys]
  (cond
    (nil? less) greater
    (nil? greater) less
    :else
    (let [l ^AZNode less
          g ^AZNode greater]
      (if (outranks? (.-fingerprint-high l) (.-fingerprint-low l) (.-entry-key l)
                     (.-fingerprint-high g) (.-fingerprint-low g) (.-entry-key g)
                     compare-keys)
        (make-node (.-entry-key l) (.-entry-value l)
                   (.-fingerprint-high l) (.-fingerprint-low l)
                   (.-left l) (zip (.-right l) greater compare-keys))
        (make-node (.-entry-key g) (.-entry-value g)
                   (.-fingerprint-high g) (.-fingerprint-low g)
                   (zip less (.-left g) compare-keys) (.-right g))))))

(defn- node-insert
  [node insert-key insert-value fingerprint-high fingerprint-low compare-keys]
  (let [n ^AZNode node
        placement (cond
                    (nil? node) :place
                    (outranks? fingerprint-high fingerprint-low insert-key
                               (.-fingerprint-high n) (.-fingerprint-low n) (.-entry-key n)
                               compare-keys) :place
                    (neg? (compare-keys insert-key (.-entry-key n))) :go-left
                    :else :go-right)]
    (case placement
      :place
      (let [[less greater] (unzip node insert-key compare-keys)]
        (make-node insert-key insert-value fingerprint-high fingerprint-low less greater))

      :go-left
      (make-node (.-entry-key n) (.-entry-value n)
                 (.-fingerprint-high n) (.-fingerprint-low n)
                 (node-insert (.-left n) insert-key insert-value fingerprint-high fingerprint-low compare-keys)
                 (.-right n))

      :go-right
      (make-node (.-entry-key n) (.-entry-value n)
                 (.-fingerprint-high n) (.-fingerprint-low n)
                 (.-left n)
                 (node-insert (.-right n) insert-key insert-value fingerprint-high fingerprint-low compare-keys)))))

(defn- node-remove
  [node remove-key compare-keys]
  (let [n ^AZNode node
        action (cond
                 (nil? node) :missing
                 (zero? (compare-keys remove-key (.-entry-key n))) :remove-here
                 (neg? (compare-keys remove-key (.-entry-key n))) :go-left
                 :else :go-right)]
    (case action
      :missing
      node

      :remove-here
      (zip (.-left n) (.-right n) compare-keys)

      :go-left
      (let [new-left (node-remove (.-left n) remove-key compare-keys)]
        (if (identical? new-left (.-left n))
          node
          (make-node (.-entry-key n) (.-entry-value n)
                     (.-fingerprint-high n) (.-fingerprint-low n)
                     new-left (.-right n))))

      :go-right
      (let [new-right (node-remove (.-right n) remove-key compare-keys)]
        (if (identical? new-right (.-right n))
          node
          (make-node (.-entry-key n) (.-entry-value n)
                     (.-fingerprint-high n) (.-fingerprint-low n)
                     (.-left n) new-right))))))

(defn- node-get [node lookup-key compare-keys not-found]
  (loop [node node]
    (if (nil? node)
      not-found
      (let [n ^AZNode node
            comparison (compare-keys lookup-key (.-entry-key n))]
        (cond
          (zero? comparison) (.-entry-value n)
          (neg? comparison) (recur (.-left n))
          :else (recur (.-right n)))))))

(defn- prefix-fingerprint
  [node boundary-key compare-keys accumulator-high accumulator-low]
  (if (nil? node)
    [accumulator-high accumulator-low]
    (let [n ^AZNode node
          comparison (compare-keys boundary-key (.-entry-key n))]
      (cond
        (neg? comparison)
        (prefix-fingerprint (.-left n) boundary-key compare-keys accumulator-high accumulator-low)

        (pos? comparison)
        (prefix-fingerprint (.-right n) boundary-key compare-keys
                            (bit-xor accumulator-high (node-aggregate-high (.-left n)) (.-fingerprint-high n))
                            (bit-xor accumulator-low (node-aggregate-low (.-left n)) (.-fingerprint-low n)))

        :else
        [(bit-xor accumulator-high (node-aggregate-high (.-left n)))
         (bit-xor accumulator-low (node-aggregate-low (.-left n)))]))))

(defn- prefix-count
  [node boundary-key compare-keys accumulator]
  (if (nil? node)
    accumulator
    (let [n ^AZNode node
          comparison (compare-keys boundary-key (.-entry-key n))]
      (cond
        (neg? comparison)
        (prefix-count (.-left n) boundary-key compare-keys accumulator)

        (pos? comparison)
        (prefix-count (.-right n) boundary-key compare-keys (+ accumulator (node-count (.-left n)) 1))

        :else
        (+ accumulator (node-count (.-left n)))))))

(defn- key-in-range? [compare-keys low high candidate-key]
  (and (or (nil? low) (<= (compare-keys low candidate-key) 0))
       (or (nil? high) (neg? (compare-keys candidate-key high)))))

(defn- subtree-overlaps-range? [compare-keys low high subtree-low subtree-high]
  (and (or (nil? high) (nil? subtree-low) (neg? (compare-keys subtree-low high)))
       (or (nil? low) (nil? subtree-high) (<= (compare-keys low subtree-high) 0))))

(defn- range-entries
  [node low high subtree-low subtree-high compare-keys accumulator]
  (cond
    (nil? node) accumulator
    (not (subtree-overlaps-range? compare-keys low high subtree-low subtree-high)) accumulator
    :else
    (let [n ^AZNode node
          node-key (.-entry-key n)
          accumulator (range-entries (.-left n) low high subtree-low node-key compare-keys accumulator)
          accumulator (if (key-in-range? compare-keys low high node-key)
                        (conj accumulator [node-key (.-entry-value n)])
                        accumulator)]
      (range-entries (.-right n) low high node-key subtree-high compare-keys accumulator))))

(defn- node-reduce-kv [node reducing-fn accumulator]
  (if (nil? node)
    accumulator
    (let [n ^AZNode node
          accumulator (node-reduce-kv (.-left n) reducing-fn accumulator)]
      (if (reduced? accumulator)
        accumulator
        (let [accumulator (reducing-fn accumulator (.-entry-key n) (.-entry-value n))]
          (if (reduced? accumulator)
            accumulator
            (node-reduce-kv (.-right n) reducing-fn accumulator)))))))

;; ── Tree wrapper ─────────────────────────────────────────────────────────────
;; assoc/dissoc/get/count/seq/reduce/reduce-kv all work on the tree, exposed via
;; each platform's own collection protocols.

(declare ->aztree)

#?(:cljd
   (deftype AZTree [root compare-keys]
     :type-only true

     IAssociative
     (-assoc [tree entry-key entry-value]
       (let [[fingerprint-high fingerprint-low] (fingerprint entry-value)
             root (node-remove root entry-key compare-keys)
             root (node-insert root entry-key entry-value fingerprint-high fingerprint-low compare-keys)]
         (->aztree root compare-keys)))

     IMap
     (-dissoc [tree entry-key]
       (->aztree (node-remove root entry-key compare-keys) compare-keys))

     ILookup
     (-lookup [tree entry-key] (node-get root entry-key compare-keys nil))
     (-lookup [tree entry-key not-found] (node-get root entry-key compare-keys not-found))
     (-contains-key? [tree entry-key]
       (not (identical? tree (node-get root entry-key compare-keys tree))))

     ICounted
     (-count [tree] (node-count root))

     IReduce
     (-reduce [tree reducing-fn]
       (node-reduce-kv root (fn [accumulator k v] (reducing-fn accumulator [k v])) nil))
     (-reduce [tree reducing-fn initial-value]
       (unreduced (node-reduce-kv root (fn [accumulator k v] (reducing-fn accumulator [k v])) initial-value)))

     IKVReduce
     (-kv-reduce [tree reducing-fn initial-value]
       (unreduced (node-reduce-kv root reducing-fn initial-value)))

     ISeqable
     (-seq [tree]
       (seq (node-reduce-kv root (fn [accumulator k v] (conj accumulator [k v])) [])))

     IEmptyableCollection
     (-empty [tree] (->aztree nil compare-keys))

     IFn
     (-invoke [tree entry-key] (node-get root entry-key compare-keys nil))
     (-invoke [tree entry-key not-found] (node-get root entry-key compare-keys not-found)))

   :clj
   (deftype AZTree [root compare-keys]
     clojure.lang.Associative
     (assoc [tree entry-key entry-value]
       (let [[fingerprint-high fingerprint-low] (fingerprint entry-value)
             root (node-remove root entry-key compare-keys)
             root (node-insert root entry-key entry-value fingerprint-high fingerprint-low compare-keys)]
         (->aztree root compare-keys)))
     (containsKey [tree entry-key]
       (not (identical? tree (node-get root entry-key compare-keys tree))))
     (entryAt [tree entry-key]
       (let [v (node-get root entry-key compare-keys tree)]
         (when-not (identical? v tree)
           (clojure.lang.MapEntry/create entry-key v))))

     clojure.lang.ILookup
     (valAt [tree entry-key] (node-get root entry-key compare-keys nil))
     (valAt [tree entry-key not-found] (node-get root entry-key compare-keys not-found))

     clojure.lang.IPersistentMap
     (without [tree entry-key]
       (->aztree (node-remove root entry-key compare-keys) compare-keys))

     clojure.lang.Counted
     (count [tree] (node-count root))

     clojure.lang.Seqable
     (seq [tree]
       (seq (node-reduce-kv root (fn [accumulator k v] (conj accumulator [k v])) [])))

     clojure.lang.IKVReduce
     (kvreduce [tree reducing-fn initial-value]
       (unreduced (node-reduce-kv root reducing-fn initial-value)))

     clojure.lang.IReduceInit
     (reduce [tree reducing-fn initial-value]
       (unreduced (node-reduce-kv root (fn [accumulator k v] (reducing-fn accumulator [k v])) initial-value)))

     clojure.lang.IFn
     (invoke [tree entry-key] (node-get root entry-key compare-keys nil))
     (invoke [tree entry-key not-found] (node-get root entry-key compare-keys not-found))))

(defn- ->aztree [root compare-keys]
  (AZTree. root compare-keys))

(defn atree
  "An empty augmented zip-zip tree ordered by `compare` (or a supplied 2-argument
   comparator). It supports the map operations assoc, dissoc, get, count, seq,
   reduce and reduce-kv, plus the negentropy range operations below. Keys are
   expected to be [timestamp entity] pairs but any comparable key works. assoc
   fingerprints its value automatically."
  ([] (atree compare))
  ([compare-keys] (->aztree nil compare-keys)))

(defn range-fingerprint
  "XOR-folded fingerprint [high low] of every entry with key in the half-open
   range [low, high), computed as prefix-fingerprint(high) XOR prefix-fingerprint(low).
   A nil bound is unbounded on that side; an empty range folds to [0 0]. This is
   what two endpoints exchange to test whether a key range matches without
   transferring its contents."
  [tree low high]
  (let [root (.-root ^AZTree tree)
        compare-keys (.-compare-keys ^AZTree tree)
        [prefix-high-high prefix-high-low] (if (nil? high)
                                             [(node-aggregate-high root) (node-aggregate-low root)]
                                             (prefix-fingerprint root high compare-keys 0 0))
        [prefix-low-high prefix-low-low] (if (nil? low)
                                           [0 0]
                                           (prefix-fingerprint root low compare-keys 0 0))]
    [(bit-xor prefix-high-high prefix-low-high)
     (bit-xor prefix-high-low prefix-low-low)]))

(defn range-size
  "Number of entries with key in the half-open range [low, high), computed as
   prefix-count(high) - prefix-count(low). A nil bound is unbounded on that side."
  [tree low high]
  (let [root (.-root ^AZTree tree)
        compare-keys (.-compare-keys ^AZTree tree)
        prefix-at-high (if (nil? high) (node-count root) (prefix-count root high compare-keys 0))
        prefix-at-low (if (nil? low) 0 (prefix-count root low compare-keys 0))]
    (- prefix-at-high prefix-at-low)))

(defn range-items
  "Ordered vector of [key value] for entries with key in [low, high). Used at the
   leaves of reconciliation once a range is small enough to send outright."
  [tree low high]
  (range-entries (.-root ^AZTree tree) low high nil nil (.-compare-keys ^AZTree tree) []))

(defn split-range
  "Divide [low, high) into at most `bucket-count` consecutive sub-ranges of roughly
   equal entry count, returned as a vector of [sub-low sub-high] bounds that cover
   [low, high) with no gaps or overlaps. Boundaries fall on actual keys, and fewer
   than `bucket-count` sub-ranges are returned when the range holds fewer entries.
   This is the recursive subdivision negentropy uses to localize a differing range."
  [tree low high bucket-count]
  (let [items (range-items tree low high)
        total (count items)]
    (if (<= total 1)
      ;; A range with 0 or 1 items is its own single bucket. An empty range still
      ;; yields [[low high]] (not []) so the responder emits an :items leaf for it:
      ;; when fingerprints differ but the responder is empty here, the initiator has
      ;; entries to push and must be told this range is a (empty) leaf to diff.
      [[low high]]
      (let [buckets (min bucket-count total)
            per-bucket (quot (+ total buckets -1) buckets)
            boundary-keys (loop [item-index per-bucket boundaries []]
                            (if (< item-index total)
                              (recur (+ item-index per-bucket) (conj boundaries (first (nth items item-index))))
                              boundaries))
            edges (concat [low] boundary-keys [high])]
        (mapv (fn [range-low range-high] [range-low range-high]) edges (rest edges))))))
