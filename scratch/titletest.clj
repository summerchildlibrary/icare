(ns titletest
  "Notification title format and registry shape, including the older bare-vector
   registration a not-yet-updated client would send."
  (:require [aztree :as aztree]
            [datamigo.push :as push]))

(defn- tree-with [completed?]
  (assoc (aztree/atree) 1 {"task-1" {:task/title "buy milk" :task/completed completed?}}))

(def diff {:puts [[2 {"task-1" {:task/title "buy milk" :task/completed true}}]]
           :deletes [1]})

(defn- check [label expected actual]
  (let [pass? (= expected actual)]
    (println (if pass? "  ok  " " FAIL ") label)
    (when-not pass? (println "        expected:" (pr-str expected) "\n        actual:  " (pr-str actual)))
    pass?))

(defn -main [& _]
  (let [results
        [(do (push/register-interests! "TOK1" {"nsA" {:name "claude" :entities ["task-1"]}})
             (check "named subscriber -> NAME completed \"TASK\""
                    [{:device-token "TOK1" :title "claude completed \"buy milk\""}]
                    (push/completion-pushes "nsA" (tree-with false) diff)))

         (do (push/register-interests! "TOK1" {})
             (push/register-interests! "TOK2" {"nsA" ["task-1"]})
             (check "bare vector (older client) still registers, falls back to Someone"
                    [{:device-token "TOK2" :title "Someone completed \"buy milk\""}]
                    (push/completion-pushes "nsA" (tree-with false) diff)))

         (do (push/register-interests! "TOK2" {})
             (push/register-interests! "TOK3" {"nsA" {:name "jun" :entities ["task-1"]}})
             (check "no body field is emitted at all"
                    [:device-token :title]
                    (vec (sort (keys (first (push/completion-pushes "nsA" (tree-with false) diff)))))))

         (do (check "already-complete task still produces nothing"
                    nil
                    (push/completion-pushes "nsA" (tree-with true) diff)))]]
    (println)
    (println (if (every? true? (map boolean results)) "ALL PASS" "FAILURES ABOVE"))))
