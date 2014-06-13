(use 'clojure.test)
(load-file "my_solution.clj")

(deftest task-01-test
  (testing "harmonic"
    (is (= (harmonic 3) 11/6)
        "harmonic: simple one")
    (is (= (harmonic 42) 12309312989335019/2844937529085600)
        "harmonic: large number"))

  (testing "digits"
    (is (= (digits 0) [0])
        "digits: zero")
    (is (= (digits 123) [1 2 3])
        "digits: normal number")
    (is (= (digits 2030) [2 0 3 0])
        "digits: number with zeroes"))

  (testing "prime-factors"
    (is (= (prime-factors 12) [2 2 3])
        "prime factors: a factor twice")
    (is (= (prime-factors 1729) [7 13 19])
        "prime factors: basic case")
    (is (= (prime-factors 337500) [2 2 3 3 3 5 5 5 5 5])
        "prime factors: larger numbers"))

  (testing "fizzbuzz"
    (is (= (fizzbuzz 3) [1 2 "fizz"])
        "fizzbuzz: fizz")
    (is (= (fizzbuzz 15) [1 2 "fizz" 4 "buzz" "fizz" 7 8 "fizz" "buzz" 11 "fizz" 13 14 "fizzbuzz"])
        "fizzbuzz: fizzbuzz"))

  (testing "uniquify"
    (is (= (uniquify ["a" "b" "c"])
           ["a" "b" "c"])
        "uniquify: no repetitions")
    (is (= (uniquify ["a" "a" "b" "b"])
           ["a" "a-1" "b" "b-1"])
        "uniquify: two elements repeated")
    (is (= (uniquify ["a" "a" "b" "b" "a"])
           ["a" "a-1" "b" "b-1" "a-2"])
        "uniquify: element repeated twice")
    (is (= (uniquify ["a" "a" "b" "a" "b" "b" "c" "a"])
           ["a" "a-1" "b" "a-2" "b-1" "b-2" "c" "a-3"])
        "uniquify: more repetition")
    (is (= (uniquify ["a" "a" "b" "a" "b" "b" "c" "a" "c"])
           ["a" "a-1" "b" "a-2" "b-1" "b-2" "c" "a-3" "c-1"])
        "uniquify: MOAR repetition"))

  (testing "densities"
    (is (= (densities [:a :a :a :b :b :c])
           [3 3 3 2 2 1])
        "densities: ordered values")
    (is (= (densities [:a :b :c :b :a :b :c :b :a])
           [3 4 2 4 3 4 2 4 3])
        "densities: unordered values"))

  (testing "index-by"
    (is (= (index-by count ["mu" "bar" "larodi"])
           {2 "mu", 3 "bar", 6 "larodi"})
        "index-by: simple case")
    (is ({{2 12, 3 33}
          {2 42, 3 33}}
         (index-by #(rem % 10) [42 12 33]))
        "index-by: two elements to the same index")))

(run-tests)
