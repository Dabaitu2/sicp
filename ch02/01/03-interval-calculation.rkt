#lang racket
;; 2.1.4 实例: 区间算数
;; 抽象的几个要点
;; 1. 像 TDD 一样的先设计用户层的数据操作，wishful thinking
;; 2. 逐层设计 abstract barrier 隔离操作和实现
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; 由于区间可能存在负数，因此我们需要考虑所有情况
(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4))))

;; x / y => x * 1 / y
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (make-interval a b)
  (cons a b))

(define (lower-bound c)
  (min (car c) (cdr c)))

(define (upper-bound c)
  (max (car c) (cdr c)))

;; (define i (make-interval 2 7))
;; (upper-bound i)
;; (lower-bound i)
;;
;; (define j (make-interval 8 3))
;; (upper-bound j)
;; (lower-bound j)


;; 使用另一种形式来描述区间
;; 指定中点和误差值，如 3.5±10
;; 它的底层仍然是区间算数
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; 通过百分比来定义区间
(define (make-center-percent c p)
  (let ([w (* (/ p 100) c)])
    (make-center-width c w)))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))
