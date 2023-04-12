#lang racket

(#%require "./generic-tools.rkt")
(#%require "./complex.rkt")
;; (#%require "./scheme-numbers.rkt")
(#%require "./rational.rkt")
(#%require "./integer.rkt")
(#%require "./real.rkt")

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
;; (install-scheme-number-package)

;; 本章我们要实现一个通用算术 package
;; 它可以同时作用于 有理数运算，复数运算，和常规的算术运算
;; 也是我们本章内容的一个大 wrap up
;; 我们相当于设置了多层的抽象屏障，并通过 data-directed programming 实现
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))

(define x (make-integer 1))
(define y (make-real 2))

(define a (make-rational 3 4))
(define b (make-rational 4 5))

(define c1 (make-complex-from-real-imag 3 4))
(define c2 (make-complex-from-mag-ang 2 1))


;; 通用的方法适用于多种数据类型, 甚至是内部类型
(add 7 b)
(add a b)
(sub x y)
(add c1 c2)
(add c1 7)
(=zero? (make-rational 2 4))

