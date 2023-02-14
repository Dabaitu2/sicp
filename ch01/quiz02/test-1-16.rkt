#lang racket
;; 迭代计算过程求快速幂
;; 由公式：(b^(n/2))^2 = (b^2)^(n/2)
;; 关键是增加一个变量缓存计算结果
(define (fast-expt-iter b counter product)
  (define (square n)
    (* n n))
  (define (even? n)
    (= (remainder n 2) 0))
  (cond
    [(= counter 0) product]
    [(even? counter)
     (fast-expt-iter (square b) (/ counter 2) product)]
    [else (fast-expt-iter b (- counter 1) (* b product))]))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(fast-expt 2 10)
