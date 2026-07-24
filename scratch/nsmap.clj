(ns nsmap)
(defn -main [& _]
  (println "default *print-namespace-maps*:" *print-namespace-maps*)
  (println "pr-str default:" (pr-str {:task/text "a" :task/done false}))
  (binding [*print-namespace-maps* false]
    (println "pr-str false:  " (pr-str {:task/text "a" :task/done false}))))
