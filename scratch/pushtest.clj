(ns pushtest
  "Exercises datamigo.push/completion-pushes: the rising-edge rule, the
   deletes-as-prior-state trick, and the cases that must stay quiet."
  (:require [aztree :as aztree]
            [datamigo.push :as push]))

(def E "task-1")
(def token "test-device-token")

(defn- tree-with [completed?]
  (assoc (aztree/atree) 42 {E {:task/title "buy milk" :task/completed completed?}}))

(defn- check [label expected actual]
  (let [pass? (if expected (seq actual) (nil? (seq actual)))]
    (println (if pass? "  ok  " " FAIL ") label "->" (pr-str actual))
    pass?))

(defn -main [& _]
  (push/register-interests! token {"nsA" [E]})
  (let [results
        [(check "rising edge fires" true
                (push/completion-pushes
                 "nsA" (tree-with false)
                 {:puts [[87 {E {:task/title "buy milk" :task/completed true}}]]
                  :deletes [42]}))

         (check "edit of an already-complete task stays quiet" false
                (push/completion-pushes
                 "nsA" (tree-with true)
                 {:puts [[88 {E {:task/title "buy oat milk" :task/completed true}}]]
                  :deletes [42]}))

         (check "bulk first sync (no deletes) stays quiet" false
                (push/completion-pushes
                 "nsA" (aztree/atree)
                 {:puts [[5 {E {:task/title "buy milk" :task/completed true}}]]
                  :deletes []}))

         (check "un-completing stays quiet" false
                (push/completion-pushes
                 "nsA" (tree-with true)
                 {:puts [[89 {E {:task/title "buy milk" :task/completed false}}]]
                  :deletes [42]}))

         (check "namespace nobody subscribed to stays quiet" false
                (push/completion-pushes
                 "nsB" (tree-with false)
                 {:puts [[87 {E {:task/title "buy milk" :task/completed true}}]]
                  :deletes [42]}))

         (check "entity nobody subscribed to stays quiet" false
                (push/completion-pushes
                 "nsA" (assoc (aztree/atree) 42 {"other-task" {:task/completed false}})
                 {:puts [[87 {"other-task" {:task/title "z" :task/completed true}}]]
                  :deletes [42]}))]]

    ;; unregistering (client drops the friend) must clear the subscription
    (push/register-interests! token {})
    (let [results (conj results
                        (check "after re-registering with no interests, quiet" false
                               (push/completion-pushes
                                "nsA" (tree-with false)
                                {:puts [[87 {E {:task/title "buy milk" :task/completed true}}]]
                                 :deletes [42]})))]
      (println)
      (println (if (every? true? (map boolean results)) "ALL PASS" "FAILURES ABOVE"))
      (shutdown-agents))))
