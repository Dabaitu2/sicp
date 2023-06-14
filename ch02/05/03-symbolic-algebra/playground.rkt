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

;; 我们可以进一步把多项式扩充为 "有理函数"  (Rational function)
;; 也就是分子和分母均为多项式的分式, 这样的有理函数也是可以做加减乘除的, 并且在最后还可以实现加减乘除后可以被化为最简形式
;;
;; quiz 2.93
;; 先简单构造出有理函数
(define p1 (make-polynomial 'x (make-termlist-of-type 'dense (list (make-term 2 1) (make-term 0 1)))))
(define p2 (make-polynomial 'x (make-termlist-of-type 'dense (list (make-term 3 1) (make-term 0 1)))))

(define rf (make-rational p2 p1))
;; 这里的 rf + rf 没有化简, 结果就非常复杂
;; (x^3 + 2 / x^2 + 1) + (x^3 + 2 / x^2 + 1) = 2 * (x^3+2) * (x^2+1) / (x^2+1) * (x^2+1)
;; 而经过重写 make-rational 到 reduce 通过求 gcd poly 进行化简之后, 结果就会自动进行化简了! Magical!
(display "after reduce: ")
(add rf rf)


;; quiz 2.94
;; 这一步是为了做化简的前置步骤，求出多项式的最大公因式, 以便未来化简
(define p3 (make-polynomial 'x (make-termlist-of-type 'dense (list (make-term 4 1) (make-term 3 -1) (make-term 2 -2) (make-term 1 2)))))
(define p4 (make-polynomial 'x (make-termlist-of-type 'dense (list (make-term 3 1) (make-term 1 -1)))))
(greatest-common-divisor p3 p4)


;; quiz 2.95
;; 本题揭示了 2.94 中存在的一个问题，涉及到多项式计算的结果可能出现非整数的情况，我们的解法还不够完善
(define p5 (make-polynomial 'x (make-termlist-of-type 'dense (list (make-term 2 1) (make-term 1 -2) (make-term 0 1)))))
(define p6 (make-polynomial 'x (make-termlist-of-type 'dense (list (make-term 2 11) (make-term 0 7)))))
(define p7 (make-polynomial 'x (make-termlist-of-type 'dense (list (make-term 1 13) (make-term 0 5)))))

(define q1 (mul p5 p6))
(define q2 (mul p5 p7))

;; 我们理想中对 q1 q2 求出 gcd 的数据结果应该是 p5, 因为很明显它是 divisor
;; 但这里 (2.96 之前) 却失败了, 因为存在浮点数计算的精度损失
(display p5)
(display " and ")
(display (greatest-common-divisor-legacy q1 q2))

;; 利用伪除, 我们可以规避浮点数精度计算的问题, 再将被放大的系数除去，即可获得正确的最大公因式
(remainder-terms (term-list q1) (term-list q2))
(pseudoremainder-terms (term-list q1) (term-list q2))
(greatest-common-divisor q1 q2)


;; 2.97 基本的算法如下，接下来做个化简就可以了
(define to-simplify (add rf rf))
(define gcdww (greatest-common-divisor ((get 'numer '(rational)) (contents to-simplify))
                                       ((get 'denom '(rational)) (contents to-simplify))))
(define hello ((get 'numer '(rational)) (contents to-simplify)))
(define world ((get 'denom '(rational)) (contents to-simplify)))

(newline)
(make-rational (car (div hello gcdww)) (car (div world gcdww)))

(reduce-terms (term-list q1) (term-list q2))
(reduce-poly q1 q2)
(reduce q1 q2)


