#lang sicp

(#%require "../02.rkt")

(define (compose f g)
  (lambda (x) (f (g x))))


;; 本质上是一种递归策略，TS 体操里面大量用到
(define (repeat f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeat f (- n 1)))))

(define dx 0.00001)

;; 生成n次平滑函数
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))
