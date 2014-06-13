(use 'clojure.test)

(load-file "my_solution.clj")

(deftest task-04-test
  (is (= (match 3
           1 :1
           2 :2
           3 :3
           4 :4)
        :3)
    "simple match")

  (is (= (match [3 4]
           [1 2] :1
           [2 3] :2
           [3 4] :3
           [4 5] :4)
        :3)
    "simple vector match")

  (is (= (match "ooops"
           1 :1
           2 :2
           _ :3
           4 :4)
        :3)
    "underscore match")

  (is (= (match [3 4]
           [1 2] :1
           [2 3] :2
           [_ 4] :3
           [4 5] :4)
        :3)
    "underscore vector match")

  (is (not (match [3 4]
             [1 2] :1
             [2 3] :2
             _     :3
             [4 5] :4))
    "underscore is matches only one thing")


  (is (match 3 :int true) ":int")
  (is (match 3 :odd true) ":odd")
  (is (match 2 :even true) ":even")
  (is (match seq :fn true) ":fn")
  (is (match [[]] :seq true) ":seq")
  (is (match "" :str true) ":str")

  (is (match 1 (:or 2 1 3) true) ":or true")
  (is (not (match 1 (:or 2 3) true)) ":or false")

  (def state (atom 0))
  (match 1
    2 (swap! state inc)
    1 true)
  (is (= @state 0) "only matched body is evaluated")

  (def state (atom 0))
  (match 101
    (swap! state inc) 0
    101               1
    (swap! state inc) 2)
  (is (= @state 1) "lazy eval patterns")

  (is (match 1
        [1 2] false
        1     true)
    "prevent partial match"))

(run-tests)
