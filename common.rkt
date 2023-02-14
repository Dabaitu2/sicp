#lang sicp

(define
  (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (average x y)
  (/ (+ x y) 2))
(define (divides? a b)
  (= (remainder b a) 0))
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(define (even? n)
  (= (remainder n 2) 0))
(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
(define (factorial n)
  (fact-iter 1 1 n))
(define (sum term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
;; 改进的寻找素数
(define (fast-find-divisor n test-divisor)
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  ;; (如果test-divisor的平方>n, 则预设自己是自己的因子,比如对于2这个test-divisor, 小于4的1,2,3肯定都是没有其他因子的
  ;; 这个地方预设了test-divisor是从最小的开始的,它只能检测到√n之前的因子(但只要√n前有因子,√n后肯定就有)
  ;; 如果一个数不能被2整除,那它肯定不是偶数,就不可能被接下来的任何偶数整除 
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (fast-find-divisor n (next test-divisor)))))

;; 从2开始就可以寻找到最小因子
(define (fast-smallest-divisor n)
  (fast-find-divisor n 2))
;; 一个数的最小因子是他自己(除了1之外)
(define (prime? n)
  (= n (fast-smallest-divisor n)))

;; 求GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(#%provide square average even? expt factorial cube sum prime? divides? gcd)