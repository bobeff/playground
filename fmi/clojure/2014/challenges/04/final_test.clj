(use 'clojure.test)

(load-file "my_solution.clj")

(deftest challenge-04-test
  (let [digits (vec "0123456789")
        letters (vec "abcdefghijklmnopqrstuvwxyz")]
    (is (= (shorten-ranges "qwerty" [\q \w \e \r \t \y]) "q..y") "simple")
    (is (= (shorten-ranges "->0123|4567" digits) "->0..3|4..7") "simple digits")
    (is (= (shorten-ranges "defghijkmnoprst" letters) "d..km..pr..t") "simple letters")
    (is (= (-> "asf123123dfghkwxyz0125678opqrstu"
               (shorten-ranges digits)
               (shorten-ranges letters))
           "asf1..31..3df..hkw..z0..25..8o..u") "complex digits and letters")
    (is (= (shorten-ranges "aabcc" (vec "aab|bcc")) "a..bcc") "first possible")
    (is (= (shorten-ranges "aaaabbbb" (vec "aaaa|bbbb|aaaabbbb")) "a..b") "best possible")
    (is (= (shorten-ranges "baaaaaa" (vec "baaPPaaaaaaaaaa")) "b..aa..a") "traversing from left to right")))

(run-tests)
