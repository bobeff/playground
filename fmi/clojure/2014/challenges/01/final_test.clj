(use 'clojure.test)

(load-file "my_solution.clj")

(deftest challenge-01-test
  (testing "from-digits"
    (is (= (from-digits []) 0)) 
    (is (= (from-digits [0]) 0)) 
    (is (= (from-digits [4 2]) 42)) 
    (is (= (from-digits [1 0 3 0]) 1030))))

(run-tests)
