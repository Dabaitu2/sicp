#lang sicp
(#%require "../../../common/math/num-theory.rkt")

;; 方案1 利用了如下的三角关系
;; x = rcosA  r = √x^2+y^2
;; y = rsinA  A = arctan(y, x)
;; 这里的反正切函数由 scheme 给出 返回正切值是 y/x 所对应的角度, 正切就是幅角的对边比邻边 y/x
;; 那么角度 A = arctan(y, x)
;; r 就是模长

(define (real-part z)
  (car z))
(define (imag-part z)
  (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y)
  (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

(#%provide real-part
           imag-part
           magnitude
           angle
           make-from-mag-ang
           make-from-real-imag)
