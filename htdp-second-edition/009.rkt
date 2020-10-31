; Exercise 9. Add the following line to the definitions area of DrRacket:
;
;    (define in ...)
;
; Then create an expression that converts the value of in to a non negative
; number.
;    - For a String, it determines how long the String is.
;    - For an Image, it uses the area.
;    - For a Number, it takes its absolute value.
;    - For #true it uses 1 and for #false 0.

#lang racket

(require 2htdp/image)

(include "data/cat-image.rkt")

(define (abs-complex complex-number)
  (sqrt (+ (sqr (real-part complex-number)) (sqr (imag-part complex-number)))))

(define (to-non-negative-number in)
  (cond
    ((string? in) (string-length in))
    ((image? in) (* (image-width in) (image-height in)))
    ((number? in) (if (complex? in) (abs-complex in) (abs in)))
    ((boolean? in) (if in 1 0))))

(to-non-negative-number "")
(to-non-negative-number "Hello Dr. Racket.")
(to-non-negative-number cat)
(to-non-negative-number -13)
(to-non-negative-number 0)
(to-non-negative-number 42)
(to-non-negative-number 3+4i)
(to-non-negative-number #false)
(to-non-negative-number #true)
