#lang sicp
;; 用类似求快速幂的方法求乘积(迭代计算方法)
;; 迭代计算方法都有一种符合尾递归的特性，也就是不用保存外部环境，因为不会再返回调用栈
;; 所以写多了发现都长的差不多..特点就是操作在递归步骤的参数中
(define (even? n)
  (= (remainder n 2) 0))
(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))
(define (multi-iter b counter product)
  (cond ((= 0 counter) product)
        ((even? b) (multi-iter (double b) (halve counter) product))
        (else (multi-iter b (- counter 1) (+ b product)))))
(define (multi a b)
  (multi-iter a b 0))
(multi 3 9)