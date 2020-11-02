; Exercise 15. Define ==>. The function consumes two Boolean values, call them
; sunny and friday. Its answer is #true if sunny is false or friday is true.
; Note Logicians call this Boolean operation implication, and they use the
; notation sunny => friday for this purpose.

#lang racket

(require rackunit)

(define (==> p q)
  (or (not p) q))

(check-true (==> #false #false))
(check-true (==> #false #true))
(check-false (==> #true #false))
(check-true (==> #true #true))
