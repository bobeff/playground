(use 'clojure.test)

(load-file "my_solution.clj")

(deftest test-chase
  (is (= (take 3 (chase 1 [1])) [1 1 1]))
  (is (= (take 5 (chase 2 [1])) [1 1 1 1 1]))
  (is (= (take 6 (chase 3 [1 2])) [1 2 1 2 1 2]))
  (is (= (take 10 (chase 4 [1 2 3 4 5])) [4 3 2 1 5 4 3 2 1 5]))
  (is (= (take 5 (chase 5 [1 2 3])) [2 1 3 2 1])))
 
(run-tests)
