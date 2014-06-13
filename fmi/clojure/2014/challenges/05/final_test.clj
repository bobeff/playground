(use 'clojure.test)

(load-file "my_solution.clj")

(deftest challenge-05-test
  (let [state (atom 0)
        coll (lazy
               (+ 1 2)
               (swap! state inc)
               (swap! state inc)
               (swap! state inc)
               (/ 1 0))]
    (is (not (realized? coll)) "lazy")
    (is (= (first coll) 3) "first")
    (is (= 0 @state) "zero")

    (second coll)
    (is (= 1 @state) "one")

    (nth coll 2)
    (is (= 2 @state) "two")

    (nth coll 3)
    (is (= 3 @state) "three")))

(run-tests)
