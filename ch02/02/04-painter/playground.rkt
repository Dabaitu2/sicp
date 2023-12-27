#lang sicp

(#%require "./frame.rkt")
(#%require "./vector.rkt")
(#%require "./painters.rkt")
(#%require "./transform.rkt")

;; helpers
(define picture-size 300)
(define frame (make-frame (make-vect 0 0)
                          (make-vect picture-size 0.0)
                          (make-vect 0.0 picture-size)))

(define flipped-wave (flip-vert (beside wave wave)))
(flipped-wave frame)

