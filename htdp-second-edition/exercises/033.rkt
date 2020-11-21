; Exercise 33. Research the “year 2000” problem.

#lang racket

(require net/url)
(require 2htdp/batch-io)

(define URL-TO-DOWNLOAD
  (string->url "https://simple.wikipedia.org/wiki/Year_2000_problem"))

(write-file "year-2000-problem.html"
            (port->string (get-pure-port URL-TO-DOWNLOAD)))
