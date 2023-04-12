#lang racket

;; 实现有理数的四则运算 a/b -> 有理数
;; numer 求分子
;; denom 求分母
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y)) (* (numer x) (denom y))))

;; cons = Construct 表示构造
;; car = Contents of Address part of Register 存储地址中的“地址”
;; cdr = Contents of Decrement part of Register 存储地址中的“减量”
;; 但在这里他们就是为了取出 Cons 的前半部分和后半部分的
;; (car x)
;; (cdr x)

;; 通过序对来模拟有理数的整数部分和小数部分
;; 获取序对分子分母的最大公约数，然后将分子分母化简为真分数
(define (make-rat n d)
  (let ([g ((if (< d 0) - +) (gcd n d))])
    (cons (/ n g) (/ d g))))

(define (numer x)
  (car x))
(define (denom x)
  (cdr x))

;; 打印出有理数为小数表示
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; (define one-half (make-rat -1 2))
;; (define one-third (make-rat 1 3))
;; (print-rat one-half)
;; (print-rat (add-rat one-half one-third))
;; (print-rat (mul-rat one-half one-third))
(#%provide make-rat
           numer
           denom
           add-rat
           sub-rat
           mul-rat
           div-rat
           print-rat)