; Sample Problem: Design a program that moves a car from left to right on the
; world canvas, three pixels per clock tick.

; Exercise 43. Let’s work through the same problem statement with a time-based
; data definition:
;   - An AnimationState is a Number.
;   - "Interpretation" the number of clock ticks since the animation started.
;
; Like the original data definition, this one also equates the states of the
; world with the class of numbers. Its interpretation, however, explains that
; the number means something entirely different.
;
; Design the functions tock and render. Then develop a big-bang expression so
; that once again you get an animation of a car traveling from left to right
; across the world’s canvas.
; 
; Use the data definition to design a program that moves the car according to a
; sine wave. (Don’t try to drive like that.)

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
; WorldState is the number of clock ticks since the animation started.
; Launches the program from some initial world state "ws".
(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-tick clock-event-handler]
    [stop-when stop?]))

; WorldState -> Number
; Calculates the car position according to the given number of clock ticks and
; car's velocity.
; examples:
;   given: 0, expect: 0
;   given: 1, expect: CAR-VELOCITY
;   given: 2, expect: CAR-VELOCITY * 2
(define (car-position ws)
  (* ws CAR-VELOCITY))

(check-equal? (car-position 0) 0)
(check-equal? (car-position 1) CAR-VELOCITY)
(check-equal? (car-position 2) (* 2 CAR-VELOCITY))

; WorldState -> Image
; Places the car into the BACKGROUND scene, according to the given world state.
(define (render cw)
  (let ([car-x-coord (car-position cw)])
    (place-image/align
      CAR car-x-coord (+ CAR-Y-COORD (sin car-x-coord))
      "right" "middle" BACKGROUND)))

; WorldState -> WorldState
; Moves the car by 3 pixels for every clock tick.
; examples:
;   given: 0, expect: 1
;   given: 20, expect: 21
;   given: 78, expect: 79
(define (clock-event-handler cw)
  (+ cw 1))

(check-equal? (clock-event-handler 0) 1)
(check-equal? (clock-event-handler 20) 21)
(check-equal? (clock-event-handler 78) 79)

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
