#lang sicp
;; 迭代计算过程求快速幂
;; 由公式：(b^(n/2))^2 = (b^2)^(n/2)
(define (square n)
  (* n n))
(define (even? n)
  (= (remainder n 2) 0))
(define (fast-expt-iter b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (fast-expt-iter (square b)
                                         (/ counter 2)
                                         product))
        (else (fast-expt-iter b
                              (- counter 1)
                              (* b product)))))
(fast-expt-iter 2 10 1)