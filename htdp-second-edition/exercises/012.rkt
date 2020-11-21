; Exercise 12. Define the function cvolume, which accepts the length of a side
; of an equilateral cube and computes its volume. If you have time, consider
; defining csurface, too.

#lang racket

(require rackunit)

(define (cvolume side)
  (* side side side))

(define (csurface side)
  (* 6 (sqr side)))

(check-equal? (cvolume 1) 1)
(check-equal? (cvolume 2) 8)
(check-equal? (cvolume 3) 27)
(check-equal? (cvolume 4) 64)
(check-equal? (cvolume 5) 125)
(check-equal? (cvolume 6) 216)
(check-equal? (cvolume 7) 343)

(check-equal? (csurface 1) 6)
(check-equal? (csurface 2) 24)
(check-equal? (csurface 3) 54)
(check-equal? (csurface 4) 96)
(check-equal? (csurface 5) 150)
(check-equal? (csurface 6) 216)
(check-equal? (csurface 7) 294)
