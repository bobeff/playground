; Exercise 38. Design the function string-remove-last, which produces a
; string like the given one with the last character removed.

#lang racket

(require rackunit)

; string (non-empty) -> string (one character shorter than input one)
; By given string, returns a new one without input's last character.
; given "", expected: error
; given "a", expected: ""
; givel "hello", expected: "hell"
(define (string-remove-last s)
  (substring s 0 (sub1 (string-length s))))

(check-exn exn:fail? (thunk (string-remove-last "")))
(check-equal? (string-remove-last "a") "")
(check-equal? (string-remove-last "hello") "hell")
