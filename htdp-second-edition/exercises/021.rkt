; Exercise 21. Use DrRacket’s stepper to evaluate (ff (ff 1)) step-by-step.
; Also try (+ (ff 1) (ff 1)). Does DrRacket’s stepper reuse the results of
; computations? 

#lang racket

(define (ff a) (* 10 a))

(ff (+ 1 1))      ; -> (ff 2) -> (* 10 2) -> 20
(ff (ff 1))       ; -> (ff (* 10 1)) -> (ff 10) -> (* 10 10) -> 100
(+ (ff 1) (ff 1)) ; -> (+ (* 10 1) (ff 1)) -> (+ 10 (ff 1)) -> (+ 10 (* 10 1))
                  ; -> (+ 10 10) -> 20

; The results of computations aren't being reused when using BSL.
