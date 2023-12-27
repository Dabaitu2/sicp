#lang sicp

(#%require "../03-symbolic-algebra/complex.rkt")
(#%require "../03-symbolic-algebra/rational.rkt")
(#%require "../03-symbolic-algebra/integer.rkt")
(#%require "../03-symbolic-algebra/real.rkt")
(#%require "../03-symbolic-algebra/api.rkt")
(#%require "../03-symbolic-algebra/poly-with-canonical.rkt")

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-polynomial-package)

;; quiz 2.95
(define p5 (make-polynomial 'x (make-termlist-of-type 'dense (list (make-term 2 1) (make-term 1 -2) (make-term 0 1)))))
(define p6 (make-polynomial 'x (make-termlist-of-type 'dense (list (make-term 2 11) (make-term 0 7)))))
(define p7 (make-polynomial 'x (make-termlist-of-type 'dense (list (make-term 1 13) (make-term 0 5)))))

(define q1 (mul p5 p6))
(define q2 (mul p5 p7))

;; 我们理想中对 q1 q2 求出 gcd 的数据结果应该是 p5, 因为很明显它是 divisor
;; 但这里却失败了, 这是因为我们在做多项式计算的时候也会去计算系数 division
;; 而一旦涉及到非整数的计算的时候会出现精度丢失和舍入
(display p5)
(display " and ")
(display (greatest-common-divisor q1 q2))

