; Exercise 45: Design a “virtual cat” world program that continuously moves the
; cat from left to right. Let’s call it cat-prog and let’s assume it consumes
; the starting position of the cat. Furthermore, make the cat move three pixels
; per clock tick. Whenever the cat disappears on the right, it reappears on the
; left. You may wish to read up on the modulo function.

; Exercise 46. Improve the cat animation with a slightly different image.
; Adjust the rendering function from exercise 45 so that it uses one cat image
; or the other based on whether the x-coordinate is odd. Read up on odd? in the
; HelpDesk, and use a cond expression to select cat images.

#lang racket

(require rackunit 2htdp/image 2htdp/universe)

(include "../data/cat-images.rkt")

(define IMAGE-WIDTH (image-width cat1))
(define IMAGE-HEIGHT (image-height cat1))
(define SCENE-WIDTH 900)
(define SCENE-HEIGHT (+ IMAGE-HEIGHT 2))
(define EXTENDED-SCENE-WIDTH (+ SCENE-WIDTH IMAGE-WIDTH))
(define CAT-Y-POSITION 1)

; WorldState -> WorldState
; "WorldState" is the number of pixels from the left corner of the scene.
; Launches the program for some initial world state "ws".
(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-tick on-tick-handler]))

; WorldState -> image
; For given "WorldState" with meaning number of pixels from the left corner of
; the scene, produces an image of cat with right corner pixels equal to the
; world state. Alternates the two cat images depending on whether world state is
; odd or even number.
(define (render ws)
  (let ([image (if (odd? ws) cat1 cat2)])
    (place-image/align image ws CAT-Y-POSITION "right" "top"
                       (empty-scene SCENE-WIDTH SCENE-HEIGHT))))

; WorldState -> WorldState
; Updates the world state with increasing 3 pixels per clock tick. When the
; value become greater than SCENE-WIDTH + IMAGE-WIDTH wraps around.
(define (on-tick-handler ws)
  (modulo (+ ws 3) EXTENDED-SCENE-WIDTH))

(check-equal? (on-tick-handler 0) 3)
(check-equal? (on-tick-handler 1) 4)
(check-equal? (on-tick-handler (- EXTENDED-SCENE-WIDTH 3)) 0)
(check-equal? (on-tick-handler EXTENDED-SCENE-WIDTH) 3)
(check-equal? (on-tick-handler (+ 1 EXTENDED-SCENE-WIDTH)) 4)

(main 0)
