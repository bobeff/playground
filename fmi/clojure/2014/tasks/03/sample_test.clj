(use 'clojure.test)

(load-file "my_solution.clj")

(deftest task-03-sample-test
  (def snake {:dir [0 1]
              :width 10
              :height 10
              :prizes #{[3 8] [1 8]}
              :location [[3 3] [3 4] [3 5] [3 6]]})
  ;; move
  (is (= (move snake) {:dir [0 1]
                       :width 10
                       :height 10
                       :prizes #{[3 8] [1 8]}
                       :location [[3 4] [3 5] [3 6] [3 7]]}))
  (is (= (move {:dir [0 1]
                :width  7
                :height 7
                :prizes #{[3 1]}
                :location [[3 3] [3 4] [3 5] [3 6]]})
         false))

  ;; danger?
  (is (= (danger? snake) false))
  (is (= (danger? {:dir [0 1]
                   :width  10
                   :height 10
                   :prizes #{[3 7]}
                   :location [[3 5] [3 6] [3 7] [3 8] [3 9]]})
         true))

  ;; turn
  (is (= (:dir (turn snake :right)) [1 0])))

(run-tests)
