(use 'clojure.test)

(load-file "my_solution.clj")

(deftest challenge-03-test
  (let [lazy-seq-class clojure.lang.LazySeq
        coll-1 [1 2 3 4 5]
        coll-2 [2]
        coll-3 (range 1 100000)
        expected-1 [2 4 1 3]
        expected-2 [3 1 4 2 5 3 1]
        expected-3 [2 2 2]
        expected-4 [512 1024 1536 2048 2560 3072 3584 4096 4608 5120]]
    (is (instance? lazy-seq-class (chase 2 coll-1)) "simple lazy-seq")
    (is (instance? lazy-seq-class (chase 8 coll-1)) "cyclic lazy-seq")
    (is (instance? lazy-seq-class (chase 3 coll-2)) "repeative lazy-seq")
    (is (instance? lazy-seq-class (chase 10 coll-3)) "range lazy-seq")
    (is (= (take 4 (chase 2 coll-1)) expected-1) "simple collection")
    (is (= (take 7 (chase 8 coll-1)) expected-2) "cyclic collection")
    (is (= (take 3 (chase 42 coll-2)) expected-3) "repeative collection")
    (is (= (take 10 (chase 512 coll-3)) expected-4) "range collection")))

(run-tests)
