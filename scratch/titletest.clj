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
             (check "named subscriber -> NAME finished \"TASK\""
                    [{:device-token "TOK1" :title "Claude finished \"buy milk\""}]
                    (push/completion-pushes "nsA" (tree-with false) diff)))

         (do (push/register-interests! "TOK1" {})
             (push/register-interests! "TOK2" {"nsA" ["task-1"]})
             (check "bare vector (older client) still registers, falls back to Someone"
                    [{:device-token "TOK2" :title "Someone finished \"buy milk\""}]
                    (push/completion-pushes "nsA" (tree-with false) diff)))

         (do (push/register-interests! "TOK2" {})
             (push/register-interests! "TOK3" {"nsA" {:name "jun" :entities ["task-1"]}})
             (check "no body field is emitted at all"
                    [:device-token :title]
                    (vec (sort (keys (first (push/completion-pushes "nsA" (tree-with false) diff)))))))

         (do (push/register-interests! "TOK3" {})
             (push/register-interests! "TOK4" {"nsA" {:name "JB" :entities ["task-1"]}})
             (check "an already-upper name is left alone, not lower-cased to Jb"
                    [{:device-token "TOK4" :title "JB finished \"buy milk\""}]
                    (push/completion-pushes "nsA" (tree-with false) diff)))

         (do (push/register-interests! "TOK4" {})
             (push/register-interests! "TOK5" {"nsA" {:name "mary jane" :entities ["task-1"]}})
             (check "only the first letter is touched"
                    [{:device-token "TOK5" :title "Mary jane finished \"buy milk\""}]
                    (push/completion-pushes "nsA" (tree-with false) diff)))

         (do (push/register-interests! "TOK5" {})
             (push/register-interests! "TOK6" {"nsA" {:name "" :entities ["task-1"]}})
             (check "an empty name does not blow up"
                    [{:device-token "TOK6" :title " finished \"buy milk\""}]
                    (push/completion-pushes "nsA" (tree-with false) diff)))

         (do (push/register-interests! "TOK6" {})
             (push/register-interests! "TOK7" {"nsA" {:name "claude" :entities ["task-1"]}})
             (check "already-complete task still produces nothing"
                    nil
                    (push/completion-pushes "nsA" (tree-with true) diff)))]]
    (println)
    (println (if (every? true? (map boolean results)) "ALL PASS" "FAILURES ABOVE"))))
