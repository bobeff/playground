; Exercise 24. Here is the definition of ==>: y
;
;    (define (==> x y)
;      (or (not x) y))
;
; Use the stepper to determine the value of (==> #true #false).

#lang racket

(define (==> x y)
  (or (not x) y))

(==> #true #false) ; -> (or (not #true) #false) -> (or #false #false) -> #false
