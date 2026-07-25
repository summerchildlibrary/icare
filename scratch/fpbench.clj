(ns fpbench
  (:require [aztree :as aztree]))

(def value {"entity-42" {:task/title "some task title number 42"
                         :task/completed true
                         :task/subitems []}})

(defn -main [& _]
  ;; cost of one fingerprint (pr-str + murmur3) on a realistic entry value
  (dotimes [_ 1000] (aztree/fingerprint value))
  (let [n 20000 start (System/nanoTime)]
    (dotimes [_ n] (aztree/fingerprint value))
    (let [us (/ (- (System/nanoTime) start) 1000.0 n)]
      (println (format "fingerprint of one entry value: %.2f µs" us))
      (println)
      (println "added cost of the fingerprint leaf diff:")
      (println "  leaf bucket is <= 4 items, both sides -> <= 8 fingerprints per leaf")
      (println (format "  per leaf visited:            %.1f µs" (* 8 us)))
      (println (format "  1 differing entry:           %.1f µs  (1 leaf)" (* 8 us)))
      (println (format "  100 differing entries:       %.1f µs  (~25 leaves)" (* 8 us 25)))
      (println (format "  1000 differing entries:      %.2f ms  (~250 leaves)" (/ (* 8 us 250) 1000.0))))))
