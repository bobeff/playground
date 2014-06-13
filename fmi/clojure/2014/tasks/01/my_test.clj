(use 'clojure.test)

(load-file "my_solution.clj")

(deftest test-digits
  (is (= (digits 0) [0]))
  (is (= (digits 3) [3]))
  (is (= (digits 31) [3 1]))
  (is (= (digits 50) [5 0]))
  (is (= (digits 501) [5 0 1]))
  (is (= (digits 5001) [5 0 0 1])))

(deftest test-prime-factors
  (is (= (prime-factors 0) []))
  (is (= (prime-factors 1) []))
  (is (= (prime-factors 2) [2]))
  (is (= (prime-factors 47) [47]))
  (is (= (prime-factors 300) [2 2 3 5 5]))
  (is (= (prime-factors 735) [3 5 7 7]))
  (is (= (prime-factors 1024) [2 2 2 2 2 2 2 2 2 2]))
  (is (= (prime-factors 21321) [3 3 23 103])))

(deftest test-fizzbuzz
  (is (= (fizzbuzz 0) []))
  (is (= (fizzbuzz 1) [1]))
  (is (= (fizzbuzz 2) [1 2]))
  (is (= (fizzbuzz 3) [1 2 "fizz"]))
  (is (= (fizzbuzz 4) [1 2 "fizz" 4]))
  (is (= (fizzbuzz 5) [1 2 "fizz" 4 "buzz"]))
  (is (= (fizzbuzz 6) [1 2 "fizz" 4 "buzz" "fizz"]))
  (is (= (fizzbuzz 7) [1 2 "fizz" 4 "buzz" "fizz" 7]))
  (is (= (fizzbuzz 8) [1 2 "fizz" 4 "buzz" "fizz" 7 8]))
  (is (= (fizzbuzz 9) [1 2 "fizz" 4 "buzz" "fizz" 7 8 "fizz"]))
  (is (= (fizzbuzz 10) [1 2 "fizz" 4 "buzz" "fizz" 7 8 "fizz" "buzz"])))

(deftest test-densities
  (is (= (densities []) []))
  (is (= (densities [1]) [1]))
  (is (= (densities [:a :b]) [1 1]))
  (is (= (densities [1 1]) [2 2]))
  (is (= (densities [3 2 3 1]) [2 1 2 1]))
  (is (= (densities [:a :b :a :c :a :b :a]) [4 2 4 1 4 2 4])))

(deftest test-index-by
  (is (= (index-by identity []) {}))
  (is (= (index-by identity [:ala :bala]) {:ala :ala, :bala :bala}))
  (is (= (index-by count ["ala" "bala" "portokala"]) {3 "ala", 4 "bala", 9 "portokala"}))
  (is (= (index-by clojure.string/upper-case ["foo", "bar"]) {"FOO" "foo", "BAR" "bar"}))
  (is (= (index-by inc [5 8 11]) {6 5, 9 8, 12 11})))

(deftest test-harmonic
  (is (= (harmonic 0) 0))
  (is (= (harmonic 1) 1))
  (is (= (harmonic 2) 3/2))
  (is (= (harmonic 3) 11/6))
  (is (= (harmonic 4) 25/12))
  (is (= (harmonic 5) 137/60)))

(deftest test-uniquify
  (is (= (uniquify []) []))
  (is (= (uniquify ["a"]) ["a"]))
  (is (= (uniquify ["a" "a"]) ["a" "a-1"]))
  (is (= (uniquify ["a" "b" "a"]) ["a" "b" "a-1"]))
  (is (= (uniquify ["a" "a" "b" "c" "b" "a"]) ["a" "a-1" "b" "c" "b-1" "a-2"]))
  (is (= (uniquify ["ala" "ala" "ala" "ala"]) ["ala" "ala-1" "ala-2" "ala-3"])))

(run-tests)
