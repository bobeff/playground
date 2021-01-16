; Nesting cond expressions can eliminate common expressions. Consider the
; function for launching a rocket, repeated in figure 24. Both branches of the
; cond expression have the same shape except as indicated with ...:
;
;    (place-image ROCKET X ... MTSCN)
;
; Reformulate create-rocket-scene.v5 to use a nested expression; the resulting
; function mentions place-image only once.

#lang racket

(require 2htdp/image)

(include "../data/rocket-image.rkt")

(define WIDTH  100)
(define HEIGHT  60)
(define MTSCN (empty-scene WIDTH HEIGHT))

(define ROCKET-CENTER-TO-TOP
  (- HEIGHT (/ (image-height ROCKET) 2)))
 
(define (create-rocket-scene.v5 h)
  (place-image ROCKET 50
               (cond
                 [(<= h ROCKET-CENTER-TO-TOP) h]
                 [(> h ROCKET-CENTER-TO-TOP) ROCKET-CENTER-TO-TOP])
               MTSCN))

(create-rocket-scene.v5 0)
(create-rocket-scene.v5 10)
(create-rocket-scene.v5 20)
(create-rocket-scene.v5 30)
(create-rocket-scene.v5 40)
