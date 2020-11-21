; Exercise 26. What do you expect as the value of this program:
;
;    (define (string-insert s i)
;      (string-append (substring s 0 i)
;                     "_"
;                     (substring s i)))
;     
;    (string-insert "helloworld" 6)
;
; Confirm your expectation with DrRacket and its stepper.

#lang racket

(require rackunit)

(define (string-insert s i)
  (string-append (substring s 0 i)
                 "_"
                 (substring s i)))
 
(check-equal? (string-insert "helloworld" 6) "hellow_orld")

; -> (string-insert "helloworld" 6)
; -> (string-append (substring "helloworld 0 6) "_" (substring "helloworld" 6))
; -> (string-append "hellow" "_" (substring "helloworld" 6))
; -> (string append "hellow" "_" "orld")
; -> "hellow_orld"
