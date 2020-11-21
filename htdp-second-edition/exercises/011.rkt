; Exercise 11. Define a function that consumes two numbers, x and y, and that
; computes the distance of point (x,y) to the origin.

#lang racket

(require rackunit)

(define (distance-from-center x y)
  (sqrt (+ (sqr x) (sqr y))))

(check-equal? (distance-from-center 3 4) 5)
(check-equal? (distance-from-center -1 -1) (sqrt 2))
(check-equal? (distance-from-center 5 12) 13)
(check-equal? (distance-from-center 68 285) 293)
