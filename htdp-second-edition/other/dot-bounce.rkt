; This is interactive program sample which moves a dot into a user supplied
; direction and bounces it from the scene bounderies.

#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define SCENE-SIZE 250)
(define HALF-SCENE-SIZE (/ SCENE-SIZE 2))
(define BACKGROUND (empty-scene SCENE-SIZE SCENE-SIZE))
(define DOT-RADIUS 3)
(define DOT (circle DOT-RADIUS "solid" "red"))

(define UP-SCENE-BOUND DOT-RADIUS)
(define DOWN-SCENE-BOUND (- SCENE-SIZE DOT-RADIUS))
(define LEFT-SCENE-BOUND DOT-RADIUS)
(define RIGHT-SCENE-BOUND (- SCENE-SIZE DOT-RADIUS))

(define-struct dot [x y move])

(define (main dot)
  (big-bang dot
    [to-draw render]
    [on-key  key-handler]
    [on-tick tick-handler]
    [stop-when stop-condition?]
    [close-on-stop #true]))

(define (render dot)
  (place-image DOT (dot-x dot) (dot-y dot) BACKGROUND))

(define (key-handler dot key-event)
  (make-dot (dot-x dot) (dot-y dot) key-event))

(define (bounce-check dot)
   (let ([x-pos (dot-x dot)]
         [y-pos (dot-y dot)])
     (cond
       [(< x-pos LEFT-SCENE-BOUND)  (make-dot LEFT-SCENE-BOUND  y-pos "right")]
       [(> x-pos RIGHT-SCENE-BOUND) (make-dot RIGHT-SCENE-BOUND y-pos "left")]
       [(< y-pos UP-SCENE-BOUND)    (make-dot x-pos UP-SCENE-BOUND    "down")]
       [(> y-pos DOWN-SCENE-BOUND)  (make-dot x-pos DOWN-SCENE-BOUND  "up")]
       [else dot])))

(define (tick-handler dot)
  (let ([x-pos (dot-x dot)]
        [y-pos (dot-y dot)]
        [move (dot-move dot)])
    (cond
      [(equal? move "left")  (bounce-check (make-dot (sub1 x-pos) y-pos move))]
      [(equal? move "right") (bounce-check (make-dot (add1 x-pos) y-pos move))]
      [(equal? move "up")    (bounce-check (make-dot x-pos (sub1 y-pos) move))]
      [(equal? move "down")  (bounce-check (make-dot x-pos (add1 y-pos) move))]
      [else dot])))

(define (stop-condition? dot)
  (equal? (dot-move dot) "escape"))

(main (make-dot HALF-SCENE-SIZE HALF-SCENE-SIZE ""))
