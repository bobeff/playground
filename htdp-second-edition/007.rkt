; Exercise 7. Boolean expressions can express some everyday problems. Suppose
; you want to decide whether today is an appropriate day to go to the mall.
; You go to the mall either if it is not sunny or if today is Friday (because
; that is when stores post new sales items).
;
; Here is how you could go about it using your new knowledge about Booleans.
; First add these two lines to the definitions area of DrRacket:
;
;    (define sunny #true)
;    (define friday #false)
;
; Now create an expression that computes whether sunny is false or friday is
; true. So in this particular case, the answer is #false. (Why?)
;
; See exercise 1 for how to create expressions in DrRacket. How many
; combinations of Booleans can you associate with sunny and friday?

#lang racket

(define sunny  #true)
(define friday #false)

(define (go-to-the-mall? is-sunny? is-friday?)
  (or (not is-sunny?) is-friday?))

; The answer is #false because the expression is being evaluated to #false when
; the two values for 'sunny' and 'friday' are substituted.
(go-to-the-mall? sunny friday)

; Four combiations of booleans can be associated with 'sunny' and 'friday'.
(go-to-the-mall? #false #false)
(go-to-the-mall? #false #true)
(go-to-the-mall? #true  #false)
(go-to-the-mall? #true  #true)
