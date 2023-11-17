#lang racket

(#%require
 "../01-stream-as-delayed-lists/delay-and-force.rkt")

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(newline)
(display "----------------------------------------")
(newline)
(stream-ref x 5)
(stream-ref x 7)
