#lang sicp
(#%require "common.rkt")
(#%require racket/trace)
;; 1-30 迭代的sum
(define (sum term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
(sum identity 1 inc 5)

;; 1.31 product 求函数值乘积
(define (product term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
(product identity 1 inc 5)

;; product 计算阶乘
(define (factorial n)
  (product identity 1 inc n))
(factorial 5)

;; product 计算π的近似值
(define (calc-pi n)
  (define (pi-term-a a)
    (cond ((= a 1) 2)
          ((even? a) (+ a 2))
          (else (+ a 1))))  ;; 奇数跟那个数刚好差一位
  (define (pi-term-b b)
    (cond ((= b 1) 3)
          ((even? b) (+ b 1))
          (else (+ b 2))))
  (* 4 
     (exact->inexact
      (/ (product pi-term-a 1 inc n)
         (product pi-term-b 1 inc n)))))
(calc-pi 100)

;; accumulate
(define (accumulate combiner init term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a init))
(accumulate + 0 identity 1 inc 5)
(accumulate * 1 identity 1 inc 5)

;; filtered-accumulate 增加过滤判断功能
(define (filtered-accumulate combiner null-value term a next b valid?)
  (define (iter a result)
    (cond ((< b a) result)
          ((valid? a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

;; 使用filtered-accumulate 求素数和
(define (sum-of-primes a b)
  (filtered-accumulate + 0 identity a inc b prime?))
(sum-of-primes 1 11)

;; 使用filtered-accumulate 求小于n的所有于n互素的正整数之和
(define (sum-of-con-primes n)
  (define (con-prime? a)
    (= 1 (gcd a n)))
  (filtered-accumulate * 1 identity 1 inc n con-prime?))
(sum-of-con-primes 10)