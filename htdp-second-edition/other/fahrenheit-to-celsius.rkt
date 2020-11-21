; Transforms temperature given in degress Fahrenheit to degrees Celsius.

#lang racket

(define (fahrenheit->celsius f)
  (* (/ 5 9) (- f  32)))

(display "Enter tempertature in degrees Fahrenheit: ")
(define temperature (read))
(display (string-append "The temperature in degrees Celsius is "
                        (real->decimal-string (fahrenheit->celsius temperature))
                        "\n"))
