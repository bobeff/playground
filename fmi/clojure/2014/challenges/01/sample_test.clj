(use 'clojure.test)

(load-file "my_solution.clj")

(deftest challenge-01-sample-test
  (is (= (from-digits [4 2]) 42)))

(run-tests)
