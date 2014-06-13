(use 'clojure.test)

(load-file "my_solution.clj")

(deftest task-04-sample-test
  (is (= (match 3
           1 :1
           2 :2
           3 :3
           4 :4)
        :3))

  (is (= (match [3 4]
           :seq  :0
           _     :1
           [2 3] :2
           [_ 4] :3
           [4 5] :4)
        :3))

  (is (= (match [3 6 1]
           [6 6 6] "0 points"
           [4 6 _] "1 point"
           [3 _ 1] "you loose")
         "you loose"))

  (is (= (match [2 nil]
           [0 (prn 0)] 0
           [1 (prn 1)] 1
           [2 (prn 2)] 2
           [2 (prn 3)] 3)
         2))

  (is (= (match '_
           0  0
           '_ 1
           _  2
           3  3)
         1))

  (is (= (match 2
           1 :one
           2 :two
           3 :three)
         :two))

  (is (= (match [2]
           [1] :one
           [2] :two
           [3] :three)
         :two))

  (is (= (match [1 2]
           [1 2] (* 3 2)
           [2 3] (prn "it is [2 3]")
           [3 4] :woooow)
         6))

  (is (= (match []
           () :list
           [] :vector)
         :list))

  (is (= (match :int
           :int 1
           _    2)
         2))

  (is (= (match [["m"]] :seq "yes") "yes"))
  (is (= (match ["m" "a" "p"] :seq "yes") nil))
  (is (= (match [["m" "a" "p"] []] [:seq :seq] "yes") "yes"))
  (is (= (match [map 1] [:fn :int] "yes") "yes"))
  (is (= (match ["map" 1] [:str :int] "yes") "yes"))
  (is (= (match ["map" 1] [_ _] "yes") "yes"))
  (is (= (match ["map" 1] _ "yes") nil))
  (is (= (match (inc 1) [:fn 1] true) nil))
  (is (= (match (range 2) [0 1] true) true))
  (is (= (match [0 1] (range 2) true) true))
  (is (= (match 3 (+ 1 2) true) true))
  (is (= (match 2 (:or 1 (inc 1) (+ 1 2)) "1, 2 or 3") "1, 2 or 3"))

  (is (= (match [1 (inc 1) 3]
           [[:or 2 1 3] 3 3]        false
           [4 _ :int]               false
           [1 (+ 2 0) (:or 4 3 19)] true
           ["a" "b" "c"]            false
           ["c" (prn 10) []]        false)
         true))

  (is true))

(run-tests)
