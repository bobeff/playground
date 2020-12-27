; Sample Problem: Design a program that moves a car from left to right on the
; world canvas, three pixels per clock tick.

; Exercise 39. Good programmers ensure that an image such as CAR can be enlarged
; or reduced via a single change to a constant definition.Good programmers
; establish a single point of control for all aspects of their programs, not
; just the graphical constants. Several chapters deal with this issue. We
; started the development of our car image with a single plain definition:
;
;  (define WHEEL-RADIUS 5)
;
; The definition of WHEEL-DISTANCE is based on the wheel’s radius. Hence,
; changing WHEEL-RADIUS from 5 to 10 doubles the size of the car image. This
; kind of program organization is dubbed single point of control, and good
; design employs this idea as much as possible.
;
; Develop your favorite image of an automobile so that WHEEL-RADIUS remains the
; single point of control.

; Exercise 40. Formulate the examples of timer event handler function usage as
; tests, that is, using the rackunit. Introduce a mistake. Re-run the tests.

; Exercise 41. Finish the sample problem and get the program to run. That is,
; assuming that you have solved exercise 39, define the constants BACKGROUND
; and Y-CAR. Then assemble all the function definitions, including their tests.
; When your program runs to your satisfaction, add a tree to the scenery. We
; used
;
; (define tree
;   (underlay/xy (circle 10 "solid" "green")
;                9 15
;                (rectangle 2 20 "solid" "brown")))
;
; to create a tree-like shape. Also add a clause to the big-bang expression
; that stops the animation when the car has disappeared on the right side. 

; Exercise 42. Modify the interpretation of the sample data definition so that
; a state denotes the x-coordinate of the right-most edge of the car.

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
