#lang sicp

(#%require "../02.rkt")

(define (compose f g)
  (lambda (x) (f (g x))))


;; 本质上是一种递归策略，TS 体操里面大量用到
(define (repeat f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeat f (- n 1)))))

((repeat square 2) 5)
