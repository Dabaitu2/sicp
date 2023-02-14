#lang sicp

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (average x y)
  (/ (+ x y) 2))
(define (divides? a b)
  (= (remainder b a) 0))
(define (abs x)
  (cond
    [(> x 0) x]
    [(= x 0) 0]
    [(< x 0) (- x)]))
(define (even? n)
  (= (remainder n 2) 0))
