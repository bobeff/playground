; Exercise 10. Now relax, eat, sleep, and then tackle the next chapter.

#lang racket

(require racket/block)

(define (life-cycle steps)
  (cond ((>= steps 0)
    (block
      (life-cycle (- steps 1))
      (let ((step (remainder steps 3)))
        (cond
          ((= step 0) (println "relax"))
          ((= step 1) (println "eat"))
          ((= step 2) (println "sleep"))))))))

(life-cycle 35)
