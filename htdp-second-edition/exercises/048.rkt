; Exercise 48. Enter the definition of reward followed by (reward 18) into the
; definitions area of DrRacket and use the stepper to find out how DrRacket
; evaluates applications of the function.

#lang racket

(define (reward s)
  (cond
    [(<= 0 s 10) "bronze"]
    [(and (< 10 s) (<= s 20)) "silver"]
    [else "gold"]))

(reward 18)

; (cond
;   [(<= 0 18 10) "bronze"]
;   [(and (< 10 18) (<= 18 20)) "silver"]
;   [else "gold"])
; ==
; (cond
;   [#false "bronze"]
;   [(and (< 10 18) (<= 18 20)) "silver"]
;   [else "gold"])
; ==
; (cond
;   [(and (< 10 18) (<= 18 20)) "silver"]
;   [else "gold"])
; ==
; (cond
;   [(and #true (<= 18 20)) "silver"]
;   [else "gold"])
; ==
; (cond
;   [(and #true #true) "silver"]
;   [else "gold"])
; ==
; (cond
;   [#true "silver"]
;   [else "gold"])
; ==
; "silver"
