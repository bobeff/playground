; Exercise 3. Add the following two lines to the definitions area:
;
;    (define str "helloworld")
;    (define i 5)
;
; Then create an expression using string primitives that adds "_" at position i.
; In general this means the resulting string is longer than the original one;
; here the expected result is "hello_world".

#lang racket

(define str "helloworld")
(define i 5)

(define (insert-underscore s i)
  (string-append (substring s 0 i) "_" (substring s i)))

(insert-underscore str i)
