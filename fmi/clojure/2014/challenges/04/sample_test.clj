(use 'clojure.test)

(load-file "my_solution.clj")

(deftest test-shorten-ranges
  (is (= (shorten-ranges "" []) ""))
  (is (= (shorten-ranges "ab" [\a \b]) "ab"))
  (is (= (shorten-ranges "abc" [\a \b \c]) "a..c"))
  (is (= (shorten-ranges "cab|abcd" (vec "dgabch")) "cab|a..cd"))
  (is (= (shorten-ranges "qwerty" [\q \w \e \r \t \y]) "q..y"))
  (is (= (shorten-ranges "->01|0123|4567" (vec "0123456789"))
         "->01|0..3|4..7"))
  (is (= (shorten-ranges "aabcc" (vec "aab|bcc")) "a..bcc"))
  (is (= (shorten-ranges "aaaabbbb" (vec "aaaa|bbbb|aaaabbbb")) "a..b"))
  (is (= (shorten-ranges "baaaaaa" (vec "baaPPaaaaaaaaaa")) "b..aa..a"))
  (is (= (shorten-ranges "aaaab" (vec "aaaa|abbbb")) "a..ab"))
  (is (= (shorten-ranges "baaaaaa" (vec "baa|aaaaaaaaaa")) "b..aa..a"))
  (is (= (shorten-ranges "baab" (vec "aab|baa")) "b..ab"))
  (is (= (shorten-ranges "-01|0123|4567aaa" (vec "012$01234567$aaa"))
         "-01|0..3|4..7a..a"))
  (is (= (shorten-ranges "aab%bcd" (vec "aab%bcc")) "a..cd"))
  (is (= (shorten-ranges "aab%bcc%|$# -> more content" (vec "aab%bcc%|$#"))
          "a..# -> more content"))
  (is (= (shorten-ranges "01234" (vec "01|1234")) "012..4"))
  (is (= (shorten-ranges "aacaa" (vec "aa|aca|aa")) "aacaa")))

(run-tests)

