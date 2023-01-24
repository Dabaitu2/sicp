#lang sicp

(define (smallest-divisor n)
  (define (square x) (* x x))
  (define (divides? a b) (= (remainder b a) 0))
  (define (find-divisor test-divisor)
    (cond [(> (square test-divisor) n) n]
          [(divides? test-divisor n) test-divisor]
          [else (find-divisor (+ test-divisor 1))]))
  (find-divisor 2))

(smallest-divisor 19999)
(smallest-divisor 1999)
(smallest-divisor 199)
