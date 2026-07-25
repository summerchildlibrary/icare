(ns leaftest
  "Reconciliation tests for the fingerprint-based leaf diff, including the
   timestamp-collision case that the old key-only diff could never repair."
  (:require [aztree :as aztree]
            [negentropy :as negentropy]))

(defn- tree-of [entries]
  (reduce (fn [t [k v]] (assoc t k v)) (aztree/atree) entries))

(defn- reconcile
  "Run the protocol to completion: initiator drives, responder answers.
   Returns [only-ours only-theirs rounds]."
  [ours theirs]
  (loop [ranges (negentropy/initial-ranges ours)
         acc-ours [] acc-theirs [] rounds 0]
    (if (or (empty? ranges) (> rounds 64))
      [acc-ours acc-theirs rounds]
      (let [sub (negentropy/respond theirs ranges)
            {:keys [next-ranges only-ours only-theirs]} (negentropy/process-response ours sub)]
        (recur next-ranges (into acc-ours only-ours) (into acc-theirs only-theirs) (inc rounds))))))

(defn- check [label pass? detail]
  (println (if pass? "  ok  " " FAIL ") label (if pass? "" (str "-> " detail)))
  pass?)

(defn -main [& _]
  (let [results
        [;; THE BUG: same key, different entity on each side (timestamp reuse)
         (let [ours   (tree-of [[0 {"task-A" {:task/title "Example task"}}]
                                [2 {:add-task-task {:task/title ""}}]])
               theirs (tree-of [[0 {:add-task-task {:task/title ""}}]
                                [2 {:add-task-task {:task/title ""}}]])
               [o t] (reconcile ours theirs)]
           (check "colliding key is detected (was invisible before)"
                  (and (some #(= 0 (first %)) o) (some #(= 0 (first %)) t))
                  {:only-ours o :only-theirs t}))

         ;; identical trees must stay silent (no needless churn)
         (let [same [[0 {"a" {:v 1}}] [5 {"b" {:v 2}}] [9 {"c" {:v 3}}]]
               [o t] (reconcile (tree-of same) (tree-of same))]
           (check "identical trees produce no diff" (and (empty? o) (empty? t)) {:o o :t t}))

         ;; plain one-sided differences still work
         (let [ours   (tree-of [[1 {"a" {:v 1}}] [2 {"b" {:v 2}}]])
               theirs (tree-of [[1 {"a" {:v 1}}]])
               [o t] (reconcile ours theirs)]
           (check "entry only we have -> only-ours"
                  (and (= 1 (count o)) (= 2 (ffirst o)) (empty? t)) {:o o :t t}))

         (let [ours   (tree-of [[1 {"a" {:v 1}}]])
               theirs (tree-of [[1 {"a" {:v 1}}] [7 {"z" {:v 9}}]])
               [o t] (reconcile ours theirs)]
           (check "entry only they have -> only-theirs"
                  (and (empty? o) (= 1 (count t)) (= 7 (ffirst t))) {:o o :t t}))

         ;; same key, same entity, different value (an edit that reused the key)
         (let [ours   (tree-of [[3 {"a" {:v "new"}}]])
               theirs (tree-of [[3 {"a" {:v "old"}}]])
               [o t] (reconcile ours theirs)]
           (check "same key, changed value is detected"
                  (and (seq o) (seq t)) {:o o :t t}))

         ;; larger tree, forcing real recursion through :fp buckets
         (let [base (mapv (fn [i] [i {(str "e" i) {:v i}}]) (range 200))
               ours (tree-of base)
               theirs (tree-of (assoc base 137 [137 {"e137" {:v :DIFFERENT}}]))
               [o t] (reconcile ours theirs)]
           (check "deep recursion finds a single divergent entry among 200"
                  (and (= 1 (count o)) (= 137 (ffirst o))
                       (= 1 (count t)) (= 137 (ffirst t)))
                  {:o o :t t}))]]
    (println)
    (println (if (every? true? (map boolean results)) "ALL PASS" "FAILURES ABOVE"))))
