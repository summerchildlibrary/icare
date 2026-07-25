(ns splittest
  "split-range must partition [low,high) into gapless, ordered, correctly-sized
   buckets — the rank-select rewrite has to match the old materializing version."
  (:require [aztree :as aztree]))

(defn- tree-of [es] (reduce (fn [t [k v]] (assoc t k v)) (aztree/atree) es))

(defn- ok [label p] (println (if p "  ok  " " FAIL ") label) p)

(defn -main [& _]
  (let [results
        (for [size [0 1 2 5 17 100 1000]
              [lo hi] [[nil nil] [10 90] [nil 50] [50 nil]]]
          (let [t (tree-of (map (fn [i] [i {:v i}]) (range size)))
                buckets (aztree/split-range t lo hi 16)
                items (aztree/range-items t lo hi)
                covered (mapcat (fn [[a b]] (aztree/range-items t a b)) buckets)]
            (and
             ;; edges chain: each bucket's high is the next bucket's low
             (ok (str "size " size " " [lo hi] " gapless")
                 (every? (fn [[[_ h] [l _]]] (= h l)) (partition 2 1 buckets)))
             ;; first/last edges match the requested range
             (ok (str "size " size " " [lo hi] " endpoints")
                 (and (= lo (ffirst buckets)) (= hi (second (last buckets)))))
             ;; buckets cover exactly the range's items, in order, no dupes
             (ok (str "size " size " " [lo hi] " covers exactly")
                 (= (vec items) (vec covered)))
             ;; never more buckets than asked for
             (ok (str "size " size " " [lo hi] " <= 16 buckets")
                 (<= (count buckets) 16)))))]
    (println)
    (println (if (every? true? results) "ALL PASS" "FAILURES ABOVE"))))
