#lang sicp

(#%require "../05-propagation-of-constraints/units.rkt")
(#%require "../05-propagation-of-constraints/connector.rkt")

;; c = 0.5 * (a + b)
;; c = 0.5 * p -> a + b
(define (average a b c)
  (let ((p (make-connector))
        (q (make-connector)))
    (adder a b p)
    (multiplier p q c)
    (constant 0.5 q)
    'ok))


(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(probe "average" c)
(probe "a" a)
(probe "b" b)

(average a b c)

(set-value! a 8 'user)

(set-value! b 10 'user)
