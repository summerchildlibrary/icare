(ns leafbench
  (:require [aztree :as aztree] [negentropy :as negentropy]))

(defn- tree-of [entries] (reduce (fn [t [k v]] (assoc t k v)) (aztree/atree) entries))

(defn- reconcile [ours theirs]
  (loop [ranges (negentropy/initial-ranges ours) o [] t [] r 0]
    (if (or (empty? ranges) (> r 64))
      [o t r]
      (let [sub (negentropy/respond theirs ranges)
            {:keys [next-ranges only-ours only-theirs]} (negentropy/process-response ours sub)]
        (recur next-ranges (into o only-ours) (into t only-theirs) (inc r))))))

(defn- entry [i]
  [i {(str "entity-" i) {:task/title (str "some task title number " i)
                         :task/completed (even? i)
                         :task/subitems []}}])

(defn- bench [label ours theirs]
  (dotimes [_ 5] (reconcile ours theirs))
  (let [n 50 start (System/nanoTime)]
    (dotimes [_ n] (reconcile ours theirs))
    (let [ms (/ (- (System/nanoTime) start) 1e6 n)
          [o t r] (reconcile ours theirs)]
      (println (format "  %-40s %7.3f ms   (%d rounds, %d+%d diffs)"
                       label ms r (count o) (count t))))))

(defn -main [& _]
  (doseq [size [100 1000]]
    (println (str "\n=== " size " entries ==="))
    (let [base   (mapv entry (range size))
          ours   (tree-of base)
          same   (tree-of base)                                   ; prebuilt
          one    (tree-of (assoc base (quot size 2) (entry -1)))  ; prebuilt
          empty- (aztree/atree)]
      (bench "in sync (no differences)"        ours same)
      (bench "1 entry differs"                 ours one)
      (bench "full divergence (worst case)"    ours empty-))))
