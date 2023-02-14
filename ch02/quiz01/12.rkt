#lang sicp

(define (make-interval a b)
  (cons a b))

(define (lower-bound c)
  (min (car c) (cdr c)))

(define (upper-bound c)
  (max (car c) (cdr c)))

(define (display-interval i)
  (newline)
  (display "[")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))

(define i (make-interval 2 7))

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
  (let ([w (* (/ p 100.0) c)])
    (make-center-width c w)))

;; 求百分比
(define (percent c)
  (* (/ (width c) (center c)) 100))

;;
(define my-interval (make-center-percent 10 50))
(lower-bound my-interval)
(upper-bound my-interval)
(center my-interval)
(percent my-interval)
