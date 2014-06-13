(use 'clojure.test)

(load-file "my_solution.clj")

(deftest test-lazy
  (let [coll-0 (lazy)
        coll-1 (lazy (/ 1 0))
        coll-2 (lazy (+ 1 2))
        coll-3 (lazy (+ 1 2) (/ 1 0))
        coll-4 (lazy (+ 1 2 3)
                     (let [x 10] (prn x) (* x x))
                     (- (fn [] -10)))]
    (is (not (realized? coll-0)))
    (is (not (realized? coll-1)))
    (is (not (realized? coll-2)))
    (is (= (first coll-2) 3))
    (is (not (realized? coll-3)))
    (is (= (first coll-3) 3))
    (is (not (realized? coll-4)))
    (is (= (first coll-4) 6))
    (is (= (second coll-4) 100))))

(run-tests)
