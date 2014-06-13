(use 'clojure.test)

(load-file "my_solution.clj")

(deftest challenge-02-test
  (is (= (fight :scissors :paper) [:scissors :cut :paper ]))
  (is (= (fight :rock :paper) [:paper :covers :rock ]))
  (is (= (fight :rock :lizard) [:rock :crushes :lizard ]))
  (is (= (fight :lizard :spock) [:lizard :poisons :spock ]))
  (is (= (fight :scissors :spock) [:spock :smashes :scissors ]))
  (is (= (fight :scissors :lizard) [:scissors :decapitate :lizard ]))
  (is (= (fight :lizard :paper) [:lizard :eats :paper ]))
  (is (= (fight :spock :paper) [:paper :disproves :spock ]))
  (is (= (fight :spock :rock) [:spock :vaporizes :rock ]))
  (is (= (fight :rock :scissors) [:rock :crushes :scissors ])))

(run-tests)
