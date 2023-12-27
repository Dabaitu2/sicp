#lang sicp
(#%require "../../../common/math/num-theory.rkt")
(#%require "./tagged.rkt")

;; 方案1 利用了如下的三角关系
;; x = rcosA  r = √x^2+y^2
;; y = rsinA  A = arctan(y, x)
;; 这里的反正切函数由 scheme 给出 返回正切值是 y/x 所对应的角度, 正切就是幅角的对边比邻边 y/x
;; 那么角度 A = arctan(y, x)
;; r 就是模长
;; 结合 tagged-data 改良后的数据表示

(define (real-part-rectangular z)
  (car z))
(define (imag-part-rectangular z)
  (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(#%provide real-part-rectangular
           imag-part-rectangular
           magnitude-rectangular
           angle-rectangular
           make-from-mag-ang-rectangular
           make-from-real-imag-rectangular)
