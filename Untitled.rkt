#lang racket
(define (square x)
  (* x x))
;; 在此处lambda作为运算符，后两个参数是运算参数
(define (f x y)
  (lambda (a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (+ 1 (* x y))
  (- 1 y))
(f 3 4)

;; 用let创建约束变量
(define (f2 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(f2 3 4)