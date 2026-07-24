(ns roundtrip (:require [clojure.edn :as edn]))
(defn -main [& _]
  (let [diff {:puts [[0 {"friend-querido-1" {:querido/name "Test Client"
                                             :querido/asset "assets/animated/heart.gif"
                                             :querido/task-order ["friend-task-1" "friend-task-2"]
                                             :position/canvas :home
                                             :position/x 200.0 :position/y 400.0}}]]
              :deletes []}
        namespace "abc" token "tok"
        s1 (pr-str [namespace diff token])
        parsed (edn/read-string s1)
        s2 (pr-str parsed)]
    (println "s1==s2 (map order stable):" (= s1 s2))
    (when (not= s1 s2) (println "S1:" s1) (println "S2:" s2))))
