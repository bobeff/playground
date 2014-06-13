(use 'clojure.test)

(load-file "my_solution.clj")

(def snake {:dir [0 1]
            :width 10
            :height 10
            :prizes #{[1 3]}
            :location [[3 3] [3 4] [3 5] [3 6]]})

(deftest test-move
  ; no prize
  (is (= (move snake)
         {:dir [0 1]
          :width 10
          :height 10
          :prizes #{[1 3]}
          :location [[3 4] [3 5] [3 6] [3 7]]}))

  ; get prize
  (is (= (get-in (move {:dir [0 1]
                        :width 10
                        :height 10
                        :prizes #{[1 3] [3 7]}
                        :location [[3 3] [3 4] [3 5] [3 6]]})
                 [:location])
         (get-in {:dir [0 1]
                  :width 10
                  :height 10
                  :prizes #{[1 3]}
                  :location [[3 3] [3 4] [3 5] [3 6] [3 7]]}
         [:location])))

  ; die - wall intersection
  (is (= (move {:dir [0 1]
                :width 7
                :height 7
                :prizes #{[3 1]}
                :location [[3 3] [3 4] [3 5] [3 6]]})
         false))

  ; die - self intersection
  (is (= (move {:dir [0 -1]
                :width 10
                :height 10
                :prizes #{}
                :location [[2 3] [3 3] [4 3] [5 3] [6 3] [6 4] [6 5] [6 6] [6 7]
                           [5 7] [4 7] [4 6] [4 5] [4 4]]})
         false))

  ; cannot bite into its tail
  (is (= (move {:dir [0 -1]
                :width 10
                :height 10
                :prizes #{}
                :location [[2 3] [3 3] [4 3] [5 3] [6 4]
                           [6 4] [6 5] [6 6] [6 7]
                           [5 7] [4 7] [3 7] [2 7]
                           [2 6] [2 5] [2 4]]})
         {:dir [0 -1]
                :width 10
                :height 10
                :prizes #{}
                :location [[3 3] [4 3] [5 3] [6 4]
                           [6 4] [6 5] [6 6] [6 7]
                           [5 7] [4 7] [3 7] [2 7]
                           [2 6] [2 5] [2 4] [2 3]]})))

(deftest test-turn
  (is (= (turn snake :left)
         {:dir [-1 0]
          :width 10
          :height 10
          :prizes #{[1 3]}
          :location [[3 3] [3 4] [3 5] [3 6]]}))

  (is (= (turn snake :right)
         {:dir [1 0]
          :width 10
          :height 10
          :prizes #{[1 3]}
          :location [[3 3] [3 4] [3 5] [3 6]]}))

  (is (= (turn snake :top)
         {:dir [0 -1]
          :width 10
          :height 10
          :prizes #{[1 3]}
          :location [[3 3] [3 4] [3 5] [3 6]]}))

  (is (= (turn snake :bottom)
         {:dir [0 1]
          :width 10
          :height 10
          :prizes #{[1 3]}
          :location [[3 3] [3 4] [3 5] [3 6]]})))

(deftest test-danger?
  (is (= (danger? snake) false))

  (is (= (danger? {:dir [0 -1]
                   :width 10
                   :height 10
                   :prizes #{}
                   :location [[2 3] [3 3] [4 3] [5 3] [6 3] [6 4] [6 5] [6 6] [6 7]
                              [5 7] [4 7] [4 6] [4 5] [4 4]]})
         true))

  (is (= (danger? {:dir [-1 0]
                   :width 5
                   :height 10
                   :prizes #{}
                   :location [[2 0] [1 0]]})
         true)))


(run-tests)
