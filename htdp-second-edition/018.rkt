; Exercise 18. Define the function string-join, which consumes two strings and
; appends them with "_" in between.

#lang racket

(require rackunit)

(define (string-join s1 s2)
  (string-append s1 "_" s2))

(check-equal? (string-join "Hello" "World") "Hello_World")
(check-equal? (string-join "" "test") "_test")
(check-equal? (string-join "test" "") "test_")
