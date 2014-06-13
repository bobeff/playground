(use 'clojure.test)

(load-file "my_solution.clj")

(deftest task-01-sample-test
  (is (= (digits 1024) [1 0 2 4]))
  (is (= (prime-factors 6) [2 3]))
  (is (= (fizzbuzz 7) [1 2 "fizz" 4 "buzz" "fizz" 7]))
  (is (= (densities [:a :a :b :b :c :a]) [3 3 2 2 1 3]))
  (is (= (index-by count ["mu" "foo" "larodi"])
         {2 "mu", 3 "foo", 6 "larodi"}))
  (is (= (harmonic 2) 3/2))
  (is (= (uniquify ["a" "b" "a" "b" "a"])
         ["a" "b" "a-1" "b-1" "a-2" ])))

(run-tests)
