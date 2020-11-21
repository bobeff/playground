; Exercise 14. Define the function string-last, which extracts the last 1String
; from a non-empty string.

#lang racket

(require rackunit)

(define (string-last s)
  (let ((lng (string-length s)))
    (substring s (- lng 1) lng)))

(check-exn exn:fail? (thunk (string-last "")))
(check-equal? (string-last "1") "1")
(check-equal? (string-last "testing") "g")
