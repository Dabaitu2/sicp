#lang racket

(#%require
 "../../../common/data/conventional-interface.rkt")
(#%require "./complex.rkt")
(#%require "./rational.rkt")
(#%require "./integer.rkt")
(#%require "./real.rkt")
(#%require "./api.rkt")
(#%require "./tag-tools.rkt")
(#%require "./env.rkt")
(#%require "./poly2.rkt")

;; Install number types
(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-polynomial-package)

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

(define tlist
  (accumulate
   adjoin-term
   (attach-tag 'sparse '())
   (list (make-term 1 3) (make-term 2 2) (make-term 3 1))))
(display tlist)

(define mpoly (make-polynomial 'x tlist))
(display mpoly)
(display c2)
