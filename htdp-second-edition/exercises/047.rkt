; Exercise 47. Design a world program that maintains and displays a “happiness
; gauge.” Let’s call it gauge-prog, and let’s agree that the program consumes
; the maximum level of happiness. The gauge display starts with the maximum
; score, and with each clock tick, happiness decreases by -0.1; it never falls
; below 0, the minimum happiness score. Every time the down arrow key is
; pressed, happiness decreases by 1/5; every time the up arrow is pressed,
; happiness jumps by 1/3.
;
; To show the level of happiness, we use a scene with a solid, red rectangle
; with a black frame. For a happiness level of 0, the red bar should be gone;
; for the maximum happiness level of 100, the bar should go all the way across
; the scene.

#lang racket

(require rackunit 2htdp/image 2htdp/universe)

(define GAUGE-WIDTH 1002)
(define GAUGE-HEIGHT 102)
(define PROGRESS-BAR-HEIGHT (- GAUGE-HEIGHT 2))

; WorldState -> WorldState
; WorldState is the gauge progress as pecent of the entire width.
(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-tick on-tick-handler]
    [on-key on-key-handler]))

; WorldState -> Image
; For given world state with meaning the gauge progress in percents produces a
; image of the gauge.
(define (render ws)
  (place-image/align
    (rectangle (calculate-progress ws) PROGRESS-BAR-HEIGHT "solid" "red")
    1 1 "left" "top"
    (empty-scene GAUGE-WIDTH GAUGE-HEIGHT)))

; WorldState -> Number
; Calculates the progress of the gauge in pixels for given world state meaning
; gauge's porgress in percents.
(define (calculate-progress ws)
  (* GAUGE-WIDTH (/ ws 100)))

(check-equal? (calculate-progress 0) 0)
(check-equal? (calculate-progress 100) GAUGE-WIDTH)
(check-equal? (calculate-progress 1) (/ GAUGE-WIDTH 100))
(check-equal? (calculate-progress 50) (/ GAUGE-WIDTH 2))

; WorldState -> WorldState
; For every clock tick decreases the progress of the gauge with 0.1%.
; The value cannot drop below 0.
(define (on-tick-handler ws)
  (if (>= ws 0.1) (- ws 0.1) 0.0))

(check-equal? (on-tick-handler 0) 0.0)
(check-equal? (on-tick-handler 0.1) 0.0)
(check-equal? (on-tick-handler 100) 99.9)

; Number -> Number
; If number < 0 => 0
; If nimber > 100 => 100
; else number
(define (clamp n)
  (cond
    [(< n 0) 0]
    [(> n 100) 100]
    [else n]))

(check-equal? (clamp -1) 0)
(check-equal? (clamp 0) 0)
(check-equal? (clamp 1) 1)
(check-equal? (clamp 99) 99)
(check-equal? (clamp 100) 100)
(check-equal? (clamp 101) 100)

; WorldState String -> WorldState
; On up key pressed increases the gauge progress with 1/3.
; On down key pressed decreases the gauge progress with 1/5.
; The value of the gauge should stay in the interval 0 to 100.
(define (on-key-handler ws key)
  (cond
    [(key=? key "up") (clamp (+ ws (/ ws 3)))]
    [(key=? key "down") (clamp (- ws (/ ws 5)))]
    [else ws]))

(check-equal? (on-key-handler 0 "up") 0)
(check-equal? (on-key-handler 0 "down") 0)
(check-equal? (on-key-handler 100 "up") 100)
(check-equal? (on-key-handler 100 "down") 80)
(check-equal? (on-key-handler 50 "up") (/ 200 3))
(check-equal? (on-key-handler 50 "down") 40)
(check-equal? (on-key-handler 50 "left") 50)

(main 100)
