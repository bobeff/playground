(use 'clojure.test)

(load-file "my_solution.clj")

(deftest challenge-03-sample-test
  (let [lazy-seq-class clojure.lang.LazySeq
        coll [1 2 3 4 5]
        expected-1 [2 4 1 3]
        expected-2 [3 1 4 2 5 3 1]]
    (is (instance? lazy-seq-class (chase 2 coll)))
    (is (instance? lazy-seq-class (chase 8 coll)))
    (is (= (take 4 (chase 2 coll)) expected-1))
    (is (= (take 7 (chase 8 coll)) expected-2))))

(run-tests)
