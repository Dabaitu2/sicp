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
(#%require "./poly-with-canonical.rkt")

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

;; Support general datum calucaltion
(add 7 b)
(add a b)
(sub x y)
(add c1 c2)
(add c1 7)
(add c2 a)
(=zero? (make-rational 2 4))

;; Support Different type of termlist
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


;; Support calculation between poly and datum
(define ypoly (make-polynomial 'y slist))
(define yterm (make-term 2 ypoly))
(add ypoly 2)


;;  Support canonical transformation
(define dlist3
  (accumulate
   adjoin-term
   (attach-tag 'dense '())
   (list (make-term 2 ypoly) (make-term 0 3))))
(define xpoly (make-polynomial 'x dlist3))
#| (y^2 + 2y + 3)x^2 + 3 |#
#| -> |#
#| x^2y^2  + (2x^2) y + (3x^2 + 3) |#
;; 转正规形式
(make-canonical 'y xpoly)

;; Support calculation between different variables polynomial: quiz 2.92
(mul ypoly xpoly)

;; quiz 2.93
(define p1 (make-polynomial 'x (make-termlist-of-type 'dense (list (make-term 2 1) (make-term 0 1)))))
(define p2 (make-polynomial 'x (make-termlist-of-type 'dense (list (make-term 3 1) (make-term 0 1)))))

(define rf (make-rational p2 p1))
(display rf)
