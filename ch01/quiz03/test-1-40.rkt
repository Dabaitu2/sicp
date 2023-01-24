#lang sicp

(#%require "../02.rkt")
(#%require "../03.rkt")

;; 求x^3+ax^2+bx+c的零点
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (get-zero-of-cubic a b c)
  (newtons-method (cubic a b c) 1))

(get-zero-of-cubic 2 2 2)
