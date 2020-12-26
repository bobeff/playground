; Exercise 34. Design the function string-first, which extracts the first
; character from a non-empty string. Don’t worry about empty strings.

#lang racket

(require rackunit)

; string (non-empty) -> string (with one character)
; Extracts the first character from a non-empty string.
; given: "", expected: error
; given: "a", expected: "a" 
; given: "hello", expected: "h"
; given: " world", expected: " "
(define (string-first s) (substring s 0 1))

(check-exn exn:fail? (thunk (string-first "")))
(check-equal? (string-first "a") "a")
(check-equal? (string-first "hello") "h")
(check-equal? (string-first " world") " ")
