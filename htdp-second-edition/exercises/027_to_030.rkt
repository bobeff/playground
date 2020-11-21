; Sample Problem: The owner of a monopolistic movie theater in a small town has
; complete freedom in setting ticket prices. The more he charges, the fewer
; people can afford tickets. The less he charges, the more it costs to run a
; show because attendance goes up. In a recent experiment the owner determined a
; relationship between the price of a ticket and average attendance.
;
; At a price of $5.00 per ticket, 120 people attend a performance. For each
; 10-cent change in the ticket price, the average attendance changes by 15
; people. That is, if the owner charges $5.10, some 105 people attend on the
; average; if the price goes down to $4.90, average attendance increases to 135.
; Let’s translate this idea into a mathematical formula:
;
;                                $(change in price)
; avg. attendance = 120 people - ------------------ . 15 people
;                                      $0.10
;
; Unfortunately, the increased attendance also comes at an increased cost. Every
; performance comes at a fixed cost of $180 to the owner plus a variable cost of
; $0.04 per attendee.
;
; The owner would like to know the exact relationship between profit and ticket
; price in order to maximize the profit. 

#lang racket

(require rackunit 2htdp/image)

; Exercise 27. Our solution to the sample problem contains several constants in
; the middle of functions. As One Program, Many Definitions already points out,
; it is best to give names to such constants so that future readers understand
; where these numbers come from. Collect all definitions in DrRacket’s
; definitions area and change them so that all magic numbers are refactored into
; constant definitions.

(define ATTENDEES-ON-5-DOLLARS 120)
(define PRICE-FOR-120-ATTENDEES 5.0)
(define ATTENDEES-CHANGE-FOR-0.1-PRICE-CHANGE 15)
(define PRICE-CHANGE-FOR-15-ATTENDEES 0.1)
(define PERFORMANCE-FIXED-COST 180)
(define PERFORMANCE-COST-PER-ATTENDEE 0.04)

; Exercise 30. Define constants for the price optimization program at the movie
; theater so that the price sensitivity of attendance (15 people for every 10
; cents) becomes a computed constant.

(define PRICE-CHANGE-FOR-ATTENDEE
  (/ ATTENDEES-CHANGE-FOR-0.1-PRICE-CHANGE
     PRICE-CHANGE-FOR-15-ATTENDEES))

(define (attendees ticket-price)
  (- ATTENDEES-ON-5-DOLLARS
     (* (- ticket-price PRICE-FOR-120-ATTENDEES)
        PRICE-CHANGE-FOR-ATTENDEE)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ PERFORMANCE-FIXED-COST
     (* PERFORMANCE-COST-PER-ATTENDEE
        (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

(define (profit-2 price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
     (+ 180
        (* 0.04
           (+ 120
              (* (/ 15 0.1)
                 (- 5.0 price)))))))

(check-equal? (profit 0) (profit-2 0))
(check-equal? (profit 1) (profit-2 1))
(check-equal? (profit 2) (profit-2 2))
(check-equal? (profit 3) (profit-2 3))
(check-equal? (profit 4) (profit-2 4))
(check-equal? (profit 5) (profit-2 5))

; Exercise 28. Determine the potential profit for these ticket prices:
; $1, $2, $3, $4, and $5. Which price maximizes the profit of the movie theater?
; Determine the best ticket price to a dime.

(profit 1)
(profit 2)
(profit 3)
; The best profit with $1 sensitivity is $1063.2 for a ticket price of $3
(profit 4)
(profit 5)

(define DIME-VALUE 0.1)

; This function calculates the best profit with a DIME-VALUE sensitivity.
(define (calc-best-profit profit-func)
  (define (calc-best-profit-impl price current-best-profit)
    (let ((profit (profit-func price))
          (next-price (+ price DIME-VALUE))
          (previous-price (- price DIME-VALUE)))
      (if (<= current-best-profit profit)
          (calc-best-profit-impl next-price profit)
          (values current-best-profit previous-price))))
  (calc-best-profit-impl 0 (profit-func 0)))

(calc-best-profit profit) 
; Calculated best possible profit when using a dime value step is $1064.1 for
; a ticket price of $2.9.

; Exercise 29. After studying the costs of a show, the owner discovered several
; ways of lowering the cost. As a result of these improvements, there is no
; longer a fixed cost; a variable cost of $1.50 per attendee remains.
;
; Modify both programs to reflect this change. When the programs are modified,
; test them again with ticket prices of $3, $4, and $5 and compare the results.

(define PERFORMANCE-FIXED-COST-NEW 0)
(define PERFORMANCE-COST-PER-ATTENDEE-NEW 1.5)

(define (cost-new ticket-price)
  (+ PERFORMANCE-FIXED-COST-NEW
     (* PERFORMANCE-COST-PER-ATTENDEE-NEW
        (attendees ticket-price))))

(define (profit-new ticket-price)
  (- (revenue ticket-price)
     (cost-new ticket-price)))

(define (profit-2-new price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
     (* 1.5
        (+ 120
           (* (/ 15 0.1)
              (- 5.0 price))))))

(profit-new 3)
(profit-new 4)
(profit-new 5)

(check-equal? (profit-new 3) (profit-2-new 3))
(check-equal? (profit-new 4) (profit-2-new 4))
(check-equal? (profit-new 5) (profit-2-new 5))

(calc-best-profit profit-new)
; Calculated best profit with a dime value step is $693 with ticket price $3.7.
