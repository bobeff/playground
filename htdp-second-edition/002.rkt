; Exercise 2. Add the following two lines to the definitions area:
;
;     (define prefix "hello")
;     (define suffix "world")
;
; Then use string primitives to create an expression that concatenates prefix
; and suffix and adds "_" between them. When you run this program, you will se
; "hello_world" in the interactions area.

#lang racket

(define prefix "hello")
(define suffix "world")

(string-append prefix "_" suffix)
 