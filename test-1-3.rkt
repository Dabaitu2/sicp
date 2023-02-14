#lang racket
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

;; 使用double将参数过程执行两遍
(define (double term)
  (lambda (x)
    (term (term x))))
(((double (double double)) inc) 5)

;; 函数组合
((compose square inc) 6)

(define (repeated f base)
;; 函数的重复运用
    (define (iter counter result)
      (if (= 1 counter)
          result
          (iter (- counter 1) (compose f result)))
      )
    (iter base f))
((repeated square 2) 5)

;; 平滑一个函数
(define (smooth f)
  (lambda (x)
    (let ((dx 0.0001))
    (/ (+
        (f (- x dx))
        (f x)
        (f (+ x dx))
        )
       3))))
((smooth square) 2)

;; 生成n次平滑函数
(define (muti-smooth f)
  (lambda (x)
    (repeated (smooth f) x)))

(define (average-damp f)
  (lambda (x)
    (average x 
             (f x))))

;; n次平均阻尼函数
(define (multi-average-damp f n)
    (repeated (average-damp f) n))
;; 返回一个过程，这个过程接受一个参数，计算这个参数的n次方，并对结果做damp-time次平均阻尼处理
(define (damp-nth-root n damp-time)
  (lambda (x)
    (fixed-point
     (multi-average-damp
       (lambda (y)
         (/ x (expt y (- n 1))))
       damp-time)
     1.0)))
;; 实验证明需要的damp次数为[lgn](对logn取整)
(define (lg n)
  (cond ((> (/ n 2) 1) (+ 1 (lg (/ n 2)))) ;; 这一步的理由是， n>2 说明 n/2 > 1，logn = log(n/2) + log(2) log(2) = 1 故可以这样递归的求解log(n/2) 
        ((< (/ n 2) 1) 0)
        (else 1)))
(define (nth-root n)
   (damp-nth-root n (lg n)))
(define sqrt (nth-root 2))
(sqrt 4)

;; 迭代式改进
(define (iterative-improve close-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (improve guess)
        (f guess))
    ((iterative-improve close-enough? improve) first-guess))

(define (another-sqrt x)
  (define dx 0.00001)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) dx))
    (define (improve guess)
        (average guess (/ x guess)))
    (define (average x y)
        (/ (+ x y) 2))
    ((iterative-improve close-enough? improve) 1.0))
