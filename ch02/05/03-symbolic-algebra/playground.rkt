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
(#%require "./poly.rkt")

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

(define slist
  (accumulate
   adjoin-term
   (attach-tag 'sparse '())
   (list (make-term 2 1) (make-term 1 2) (make-term 0 3))))

(define dlist
  (accumulate
   adjoin-term
   (attach-tag 'dense '())
   (list (make-term 2 1) (make-term 1 2) (make-term 0 3))))

(define slist2
  (accumulate
   adjoin-term
   (attach-tag 'sparse '())
   (list (make-term 5 1) (make-term 0 (sub 0 1)))))

(define dlist2
  (accumulate
   adjoin-term
   (attach-tag 'dense '())
   (list (make-term 2 1) (make-term 0 (sub 0 1)))))

;; (display slist)
;; (newline)
;; (display dlist)

(define mpoly (make-polynomial 'x dlist))
;; (display mpoly)
;; (newline)
;; (display c2)

;; (newline)
;; (display mpoly)
;; (newline)
;; (display mpoly)
;; (newline)
(add mpoly mpoly)
(sub mpoly mpoly)
(mul mpoly mpoly)

(define dpoly (make-polynomial 'x dlist2))
(define spoly (make-polynomial 'x slist2))

;; (display dpoly)
;; (display spoly)
(div spoly dpoly)
