#lang sicp

(#%require "./generic-tools.rkt")
(#%require "./complex.rkt")
(#%require "./scheme-numbers.rkt")
(#%require "./rational.rkt")


(install-complex-package)
(install-rational-package)
(install-scheme-number-package)

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

(define x (make-scheme-number 1))
(define y (make-scheme-number 2))

(define a (make-rational 3 4))
(define b (make-rational 4 5))

(define c1 (make-complex-from-real-imag 3 4))
(define c2 (make-complex-from-real-imag 2 5))

;; 通用的方法适用于多种数据类型, 甚至是内部类型
(add a b)
(add 7 8)
(sub x y)
(add c1 c2)
(=zero? (make-rational 2 4))
