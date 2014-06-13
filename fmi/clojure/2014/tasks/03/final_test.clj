(use 'clojure.test)

(load-file "my_solution.clj")

(require '[clojure.set :refer [intersection]])

(deftest task-03-test
  (testing "move"
    (is (= (move {:dir [0 1]
                  :width 10
                  :height 10
                  :prizes #{[3 8] [1 8]}
                  :location [[3 3] [3 4] [3 5] [3 6]]})
          {:dir [0 1]
           :width 10
           :height 10
           :prizes #{[3 8] [1 8]}
           :location [[3 4] [3 5] [3 6] [3 7]]})
      "move: simple")

    (let [s1 {:dir [0 1]
              :width 10
              :height 10
              :prizes #{[3 8] [3 7]}
              :location [[3 3] [3 4] [3 5] [3 6]]}
          {:keys [prizes location]} (move s1)]
      (is (not (contains? prizes [3 7]))
        "move with prize: old prize removed")
      (is (= 2 (count prizes))
        "move with prize: new prize added")
      (is (= location [[3 3] [3 4] [3 5] [3 6] [3 7]])
        "move with prize: proper location")
      (is (empty? (intersection prizes (set location)))
        "move with prize: no prizes over the snake"))

    (is (= (move {:dir [0 1]
                  :width  7
                  :height 7
                  :prizes #{[0 9]}
                  :location [[3 3] [3 4] [3 5] [3 6]]})
          false)
      "move: dying to the wall")

    (is (= (move {:dir [-1 0]
                  :width  20
                  :height 20
                  :prizes #{[9 0]}
                  :location [[1 5] [2 5] [3 5] [3 6]
                             [3 7] [3 8] [4 8] [5 8]
                             [5 7] [5 6] [4 6]]})
          false)
      "move: dying to with bite")

    (is (= (move {:dir [-1 0]
                  :width  20
                  :height 20
                  :prizes #{[0 0]}
                  :location [[6 1] [6 2] [6 3]
                             [7 3] [7 2] [7 1]]})
          {:dir [-1 0]
           :width  20
           :height 20
           :prizes #{[0 0]}
           :location [[6 2] [6 3] [7 3]
                      [7 2] [7 1] [6 1]]})
      "move: almost bite"))

  (testing "danger?"
    (is (not (danger? {:dir [0 1]
                       :width 10
                       :height 10
                       :prizes #{[3 8] [1 8]}
                       :location [[3 3] [3 4] [3 5] [3 6]]}))
      "danger?: no danger")

    (is (= (danger? {:dir [0 1]
                     :width  10
                     :height 10
                     :prizes #{[3 7]}
                     :location [[3 5] [3 6] [3 7] [3 8] [3 9]]})
          true)
      "danger?: wall")

    (is (= (danger? {:dir [0 1]
                     :width  10
                     :height 10
                     :prizes #{[3 7]}
                     :location [[3 5] [3 6] [3 7] [3 8] [4 8]]})
          true)
      "danger?: wall in two moves")

    (is (= (danger? {:dir [-1 0]
                     :width  20
                     :height 20
                     :prizes #{[3 7]}
                     :location [[3 5] [3 6] [3 7] [3 8] [4 8] [4 7]]})
          true)
      "danger?: bite")

    (is (= (danger? {:dir [-1 0]
                     :width  20
                     :height 20
                     :prizes #{[3 7]}
                     :location [[1 5] [2 5] [3 5] [3 6]
                                [3 7] [3 8] [4 8] [5 8]
                                [5 7] [5 6]]})
          true)
      "danger?: bite in two moves"))

  (testing "turn"
    (let [snake {:dir [0 1]
                 :width 10
                 :height 10
                 :prizes #{[3 8] [1 8]}
                 :location [[3 3] [3 4] [3 5] [3 6]]}]
      (is (= (:dir (turn snake :top)) [0 -1])   "turn: top")
      (is (= (:dir (turn snake :left)) [-1 0])  "turn: left")
      (is (= (:dir (turn snake :right)) [1 0])  "turn: right")
      (is (= (:dir (turn snake :bottom)) [0 1]) "turn: bottom"))))

(run-tests)
