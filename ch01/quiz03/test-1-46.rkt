#lang sicp

(#%require "../03.rkt")

;; 抽象我们之前处理问题的动作为一个通用过程
;; 称为迭代式改进
;; 返回一个过程，接受两个过程作为参数
;; 一个用来判断是不是 ok 另一个用来表示如何做下一轮迭代
;; 返回的过程接受首个猜测值
(define (iterative-improve close-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

;; 使用改进后的过程来抽象 fixed-point 和 sqrt
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
