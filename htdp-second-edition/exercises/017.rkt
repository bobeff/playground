; Exercise 17. Define the function image-classify, which consumes an image and
; conditionally produces "tall" if the image is taller than wide, "wide" if it
; is wider than tall, or "square" if its width and height are the same.

#lang racket

(require 2htdp/image rackunit)

(include "../data/cat-image.rkt")

(define (image-classify image)
  (let ((width (image-width image))
        (height (image-height image)))
  (cond
    ((< width height) "tall")
    ((> width height) "wide")
    ((= width height) "square"))))

(define tall-rectangle (rectangle 10 20 "solid" "red"))
(define wide-rectangle (rectangle 20 10 "solid" "green"))
(define square (rectangle 20 20 "solid" "blue"))

(check-equal? (image-classify cat) "tall")
(check-equal? (image-classify tall-rectangle) "tall")
(check-equal? (image-classify wide-rectangle) "wide")
(check-equal? (image-classify square) "square")
