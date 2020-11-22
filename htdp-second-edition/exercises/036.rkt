; Exercise 36. Design the function image-area, which counts the number of
; pixels in a given image.

#lang racket

(require 2htdp/image rackunit)

; image -> number of pixels
; Calculates the number of pixels in an image.
; given: dot, expected: 1
; given: square 5x5, expected: 25
; given: rectangle 5x10, expected 50
(define (image-area image)
  (* (image-width image) (image-height image)))

(define (pixel color) (square 1 "solid" color))

(check-equal? (image-area (pixel "red")) 1)
(check-equal? (image-area (square 5 "solid" "red")) 25)
(check-equal? (image-area (rectangle 5 10 "outline" "blue")) 50)
