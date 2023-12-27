#lang sicp
(#%require "../../../common/math/num-theory.rkt")
(#%require "./tagged.rkt")

;; 方案二则反过来
;; x = rcosA  r = √x^2+y^2
;; y = rsinA  A = arctan(y, x)

;; 可以注意到在实际的 selector 实现中, 传入的数据是无类型的，这意味着我们已经对处理的东西有了假设
;; 而这样的假设就是由上层的 tag 来保证，而 constructor 中我们也会将这样的信息传递给上层，由上层来识别这些信息
(define (magnitude-polar z)
  (car z))
(define (angle-polar z)
  (cdr z))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(#%provide magnitude-polar
           angle-polar
           real-part-polar
           imag-part-polar
           make-from-mag-ang-polar
           make-from-real-imag-polar)
