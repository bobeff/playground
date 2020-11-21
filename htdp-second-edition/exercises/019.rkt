; Exercise 19. Define the function string-insert, which consumes a string str
; plus a number i and inserts "_" at the ith position of str. Assume i is a
; number between 0 and the length of the given string (inclusive).

#lang racket

(require rackunit)

(define (string-insert str i)
  (string-append (substring str 0 i) "_" (substring str i (string-length str))))

(check-equal? (string-insert "HelloWorld" 5) "Hello_World")
(check-equal? (string-insert "HelloWorld" 0) "_HelloWorld")
(check-equal? (string-insert "HelloWorld" 10) "HelloWorld_")
(check-exn exn:fail? (thunk (string-insert "HelloWorld" 11)))
