#lang racket
(#%require "../../../common/math/num-theory.rkt")

;; 方案二则反过来
;; x = rcosA  r = √x^2+y^2
;; y = rsinA  A = arctan(y, x)

(define (magnitude z)
  (car z))
(define (angle z)
  (cdr z))

(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (make-from-mag-ang r a)
  (cons r a))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
