#lang racket
;; 基本的数学操作

(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

;; 阶乘
(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))
  (fact-iter 1 1 n))

;; 求 fibonacci
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;; 快速求幂法
(define (fast-expt b n)
  (cond [(= n 0) 1]
        [(even? n) (square (fast-expt b (/ n 2)))]
        [else (* b (fast-expt b (- n 1)))]))

;; GCD 求最大公约数
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b)))) ;; remainder 是求余

;; 质数检查
(define (prime? n)
  (define (smallest-divisor n)
    (define (divides? a b) (= (remainder b a) 0))
    (define (find-divisor test-divisor)
      (cond [(> (square test-divisor) n) n]
            [(divides? test-divisor n) test-divisor]
            [else (find-divisor (+ test-divisor 1))]))
    (find-divisor 2))
  (= n (smallest-divisor n)))

;; Sigma 求和
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) ;; 对 a 做一些操作
         (sum term (next a) next b)))) ;; 此处仍然是递归

;; 求定积分
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(#%provide cube
           square
           average
           factorial
           fib
           fast-expt
           prime?
           gcd
           sum
           integral)
