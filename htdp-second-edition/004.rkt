; Exercise 4. Use the same setup as in exercise 3 to create an expression that
; deletes the ith position from str. Clearly this expression creates a shorter
; string than the given one. Which values for i are legitimate?

#lang racket

(define str "helloworld")
(define i 5)

(define (remove-character s i)
  (string-append (substring s 0 i) (substring s (+ i 1))))

(remove-character str i)
