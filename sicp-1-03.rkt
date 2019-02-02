#lang sicp
(#%require "common.rkt")
;; sicp-1-03 用高阶函数做抽象
;; 计算给定范围的整数之和
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
;; 计算给定范围的立方和
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
;; 计算一个求π/8的收敛级数
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
;; 抽象出共同部分
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc x) (+ x 1))
(define (new-sum-cubes a b)
  (sum cube a inc b))
(new-sum-cubes 5 10)
(sum-cubes 5 10)
(define (identity x) x)
(define (new-sum-integers a b)
  (sum identity a inc b))
(new-sum-integers 1 5)

(define (new-pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
(new-pi-sum 1 1000)
(pi-sum 1 1000)

;; 求定积分
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.00001)
;; simpsom 积分法
(define (simpsom-intergral f a b n counter)
  (define (h) (/ (- b a) n))
  (define (co) (/ (h) 3))
  (define (get-y x)
    (f (+ a (* x (h)))))
  (define (term-simpsom x)
    (cond ((or (= x 0) (= x n)) (* (co) (get-y x)))
          ((even? x) (* (co) 2 (get-y x)))
          (else (* (co) 4 (get-y x)))))
  (cond ((not (even? n)) (display "wrong n"))
        (else (sum term-simpsom counter inc n))))
(simpsom-intergral cube 0 1 1000 0)