; Sample Problem: Design a program that moves a car from left to right on the
; world canvas, three pixels per clock tick. If the mouse is clicked anywhere
; on the canvas, the car is placed at the x-coordinate of that click.

; Exercise 44. Formulate the examples as BSL tests.

#lang racket

(require 2htdp/image 2htdp/universe rackunit)

(define WHEEL-RADIUS 10)
(define CAR-BODY-WIDTH  (* 8 WHEEL-RADIUS))
(define CAR-BODY-HEIGHT (* 2 WHEEL-RADIUS))
(define CAR-COUPE-WIDTH (* 4 WHEEL-RADIUS))
(define CAR-COUPE-HEIGHT WHEEL-RADIUS)

(define WHEEL (circle WHEEL-RADIUS "solid" "black"))

(define CAR
  (overlay/xy
    (rectangle CAR-COUPE-WIDTH CAR-COUPE-HEIGHT "solid" "red")
    (* -2 WHEEL-RADIUS)  WHEEL-RADIUS
    (overlay/xy
      WHEEL
      (- WHEEL-RADIUS) (- WHEEL-RADIUS)
      (overlay/xy
        WHEEL
        (* -5 WHEEL-RADIUS) (- WHEEL-RADIUS)
        (rectangle CAR-BODY-WIDTH CAR-BODY-HEIGHT "solid" "red")))))

(define CAR-Y-COORD (* 6 WHEEL-RADIUS))
(define CAR-VELOCITY 3)

(define TREE-CROWN-RADIUS (* 2 WHEEL-RADIUS))
(define TREE-TRUNK-HEIGHT (* 2 TREE-CROWN-RADIUS))
(define TREE-TRUNK-WIDTH (/ TREE-TRUNK-HEIGHT 10))

(define TREE
  (underlay/xy
   (circle TREE-CROWN-RADIUS "solid" "green")
   (- TREE-CROWN-RADIUS (/ TREE-TRUNK-WIDTH 2))
   (* 1.5 TREE-CROWN-RADIUS)
   (rectangle TREE-TRUNK-WIDTH TREE-TRUNK-HEIGHT "solid" "brown")))

(define SCENE-WIDTH (* 100 WHEEL-RADIUS))
(define SCENE-HEIGHT (* 8 WHEEL-RADIUS))

(define BACKGROUND
  (place-image TREE (/ SCENE-WIDTH  2) (/ SCENE-HEIGHT 2)
               (empty-scene SCENE-WIDTH SCENE-HEIGHT "white")))

; WorldState -> WorldState
; WorldState is the number of pixels from the left corner of the scene.
; Launches the program from some initial world state "ws".
(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-tick clock-event-handler]
    [on-mouse mouse-event-handler]
    [stop-when stop?]))

; WorldState -> Image
; Places the car into the BACKGROUND scene, according to the given world state.
(define (render cw)
  (place-image/align CAR cw CAR-Y-COORD "right" "middle" BACKGROUND))

; WorldState -> WorldState
; Moves the car by 3 pixels for every clock tick.
; examples:
;   given: 0, expect: CAR-VELOCITY
;   given: 20, expect: 20 + CAR-VELOCITY
;   given: 78, expect: 78 + CAR-VELOCITY
(define (clock-event-handler cw)
  (+ cw CAR-VELOCITY))

(check-equal? (clock-event-handler 0) CAR-VELOCITY)
(check-equal? (clock-event-handler 20) (+ 20 CAR-VELOCITY))
(check-equal? (clock-event-handler 78) (+ 78 CAR-VELOCITY))

; WorldState Number Number String -> WorldState
; Places the car at x-mouse if the given mouns event is "button-down".
; Examples:
;   given: 21 10 20 "enter", expected: 21
;   given: 42 10 20 "button-down", expected: 10
;   given: 42 10 20 "move", expected: 42
(define (mouse-event-handler x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) x-mouse]
    [else x-position-of-car]))

(check-equal? (mouse-event-handler 21 10 20 "enter") 21)
(check-equal? (mouse-event-handler 42 10 20 "button-down") 10)
(check-equal? (mouse-event-handler 42 10 20 "move") 42)

; WorldState -> bool
; Stops the animation when the car disapears from the scene
; examples:
;   given 0, exepct: #false
;   given SCENE-WIDTH / 2, expect: #false
;   given SCENE-WIDTH, expect: #false
;   given SCENE-WIDTH + CAR-BODY-WIDTH, expect: #true
;   given SCENE-WIDTH + CAR-BODY-WIDTH + 1, expect: #true
(define (stop? cw)
  (>= cw (+ SCENE-WIDTH CAR-BODY-WIDTH)))

(check-false (stop? 0))
(check-false (stop? (/ 2 SCENE-WIDTH)))
(check-false (stop? SCENE-WIDTH))
(check-true (stop? (+ SCENE-WIDTH CAR-BODY-WIDTH)))
(check-true (stop? (+ SCENE-WIDTH CAR-BODY-WIDTH 1)))

(main 0)
