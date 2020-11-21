; Exercise 5. Use the 2htdp/image library to create the image of a simple boat
; or tree. Make sure you can easily change the scale of the entire image. 

#lang racket

(require 2htdp/image)

(define (boat-image scale-factor)
  (scale
    scale-factor
    (overlay/xy
      (crop 5 0 5 25
      (ellipse 10 25 "solid" "mediumgray"))
      -20 0
      (overlay/xy
        (rectangle 2 30 "solid" "lightbrown")
        -19 30
        (crop 0 10 40 10
          (ellipse 40 20 "solid" "darkbrown"))))))

(define (tree-image scale-factor)
  (scale
    scale-factor
    (overlay/xy
      (triangle 30 "solid" "green")
      10 15
      (rectangle 10 30 "solid" "brown"))))

(boat-image 3)
(tree-image 3)
