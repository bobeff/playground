; Create an expression that counts the number of pixels in an image.

#lang racket

(require 2htdp/image)

(include "data/cat-image.rkt")

(define (pixels-count image)
  (* (image-width image) (image-height image)))

(pixels-count cat)
