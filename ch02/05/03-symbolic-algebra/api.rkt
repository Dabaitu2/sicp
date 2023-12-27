#lang sicp

(#%require "./generic-tools.rkt")

;; 本章我们要实现一个通用算术 package
;; 它可以同时作用于 有理数运算，复数运算，和常规的算术运算
;; 也是我们本章内容的一个大 wrap up
;; 我们相当于设置了多层的抽象屏障，并通过 data-directed programming 实现
;; 讲道理，这里的符号甚至可以直接使用 + - * /
(define (sub x y)
  (apply-generic 'sub x y))
(define (add x y)
  (apply-generic 'add x y))
(define (mul x y)
  (apply-generic 'mul x y))
(define (div x y)
  (apply-generic 'div x y))
(define (negate x)
  (apply-generic 'negate x))
(define (equ? x y)
  (apply-generic 'equ? x y))
(define (less? x y)
  (apply-generic 'less? x y))
(define (more? x y)
  (apply-generic 'more? x y))
(define (=zero? x)
  (apply-generic '=zero? x))
(define (raise x)
  (apply-generic 'raise x))
(define (sine x)
  (apply-generic 'sine x))
(define (cosine x)
  (apply-generic 'cosine x))
(define (greatest-common-divisor x y)
  (apply-generic 'greatest-common-divisor x y))
(define (greatest-common-divisor-legacy x y)
  (apply-generic 'greatest-common-divisor-legacy x y))
(define (pseudoremainder-terms t1 t2)
  (apply-generic 'pseudoremainder-terms t1 t2))
(define (remainder-terms t1 t2)
  (apply-generic 'remainder-terms t1 t2))
(define (div-terms t1 t2)
  (apply-generic 'div-terms t1 t2))
(define (reduce a b)
  (apply-generic 'reduce a b))

(#%provide sub
           add
           mul
           div
           equ?
           less?
           more?
           =zero?
           raise
           sine
           cosine
           negate
           greatest-common-divisor
           pseudoremainder-terms
           remainder-terms
           div-terms
           greatest-common-divisor-legacy
           reduce)
