; Exercise 37. Design the function string-rest, which produces a string like
; the given one with the first character removed.

#lang racket

(require rackunit)

; string (non-empty) -> string (one character shorter than the input one)
; By given string, returns a new one without the input's first character.
; given: "", expected: error
; given: "a", expected: ""
; given: "hello", expected: "ello"
(define (string-rest s)
  (substring s 1 (string-length s)))

(check-exn exn:fail? (thunk (string-rest "")))
(check-equal? (string-rest "a") "")
(check-equal? (string-rest "hello") "ello")
