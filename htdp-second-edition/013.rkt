; Exercise 13. Define the function string-first, which extracts the first
; 1String from a non-empty string.

#lang racket

(require rackunit)

(define (string-first s)
  (substring s 0 1))

(check-exn exn:fail? (thunk (string-first "")))
(check-equal? (string-first "1") "1")
(check-equal? (string-first "testing") "t")
