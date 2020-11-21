; Exercise 8. Create a conditional expression that computes whether the image
; is tall or wide. An image should be labeled "tall" if its height is larger
; than or equal to its width; otherwise it is "wide". See exercise 1 for how to
; create such expressions in DrRacket; as you experiment, replace the cat with
; a rectangle of your choice to ensure that you know the expected answer.
;
; Now try the following modification. Create an expression that computes
; whether a picture is "tall", "wide", or "square".

#lang racket

(require 2htdp/image)

(include "../data/cat-image.rkt")

(define (image-kind? image)
  (let ((width (image-width image))
        (height (image-height image)))
    (cond
      ((< width height) "tall")
      ((> width height) "wide")
      ((= width height) "square"))))

cat
(image-kind? cat)

(define tall-rectangle (rectangle 10 20 "solid" "red"))
(define wide-rectangle (rectangle 20 10 "solid" "green"))
(define square (rectangle 20 20 "solid" "blue"))

tall-rectangle
(image-kind? tall-rectangle)

wide-rectangle
(image-kind? wide-rectangle)

square
(image-kind? square)
