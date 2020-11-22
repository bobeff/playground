; Exercise 35. Design the function string-last, which extracts the last
; character from a non-empty string.

#lang racket

(require rackunit)

; string (non-empty) -> string (with on character)
; Extracts the last character from a non-empty string.
; given "", expected: error
; given "a", expected: "a"
; given "hello", expected: "o"
; given "world ", expected: " "
(define (string-last s)
  (let ([length (string-length s)])
    (substring s (- length 1) length)))

(check-exn exn:fail? (thunk (string-last "")))
(check-equal? (string-last "a") "a")
(check-equal? (string-last "hello") "o")
(check-equal? (string-last "world ") " ")
