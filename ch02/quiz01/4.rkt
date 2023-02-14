#lang sicp

;; 更加通用的 Cons 做法, 不再将实际逻辑写在 Cons 里面
;; 而是让具体 Selector 来决定怎么做
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define x (cons 3 4))
(car x)
(cdr x)
