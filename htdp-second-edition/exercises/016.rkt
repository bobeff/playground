; Exercise 16. Define the function image-area, which counts the number of
; pixels in a given image. See exercise 6 for ideas.

#lang racket

(require rackunit 2htdp/image)

(include "../data/cat-image.rkt")

(define (image-area image)
  (* (image-width image) (image-height image)))

(define rect (rectangle 10 20 "solid" "red"))
(define crcl (circle 10 "solid" "green"))

(check-equal? (image-area rect) 200)
(check-equal? (image-area crcl) 400)
(check-equal? (image-area cat) 8775)
