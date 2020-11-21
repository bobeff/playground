; Exercise 20. Define the function string-delete, which consumes a string plus
; a number i and deletes the ith position from str. Assume i is a number
; between 0 (inclusive) and the length of the given string (exclusive).
; Can string-delete deal with empty strings?

#lang racket

(require rackunit)

(define (string-delete str i)
  (string-append (substring str 0 i)
                 (substring str (+ i 1) (string-length str))))

(check-exn exn:fail? (thunk (string-delete "" 0)))
(check-equal? (string-delete "t" 0) "")
(check-exn exn:fail? (thunk (string-delete "t" 1)))
(check-equal? (string-delete "test" 0) "est")
(check-equal? (string-delete "test" 1) "tst")
(check-equal? (string-delete "test" 3) "tes")
(check-exn exn:fail? (thunk (string-delete "test" 4)))

; Can string-delete deal with empty strings?
; It deals with empty strings by rising an exception. The other possible variant
; is to check whether the index is out of range and if so to simply return the
; original string in that case.

(define (string-delete-no-exn str i)
  (let ((lng (string-length str))) 
    (if (or (< i 0) (>= i lng)) str
        (string-delete str i))))

(check-equal? (string-delete-no-exn "" 0) "")
(check-equal? (string-delete-no-exn "t" 0) "")
(check-equal? (string-delete-no-exn "t" 1) "t")
(check-equal? (string-delete-no-exn "test" 0) "est")
(check-equal? (string-delete-no-exn "test" 1) "tst")
(check-equal? (string-delete-no-exn "test" 3) "tes")
(check-equal? (string-delete-no-exn "test" 4) "test")
