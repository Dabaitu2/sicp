#lang sicp

(#%require "../05-propagation-of-constraints/units.rkt")
(#%require "../05-propagation-of-constraints/connector.rkt")

(define (squarer a b)
  (multiplier a a b))

(define a (make-connector))
(define b (make-connector))
(squarer a b)

(probe "a" a)
(probe "b" b)

;; (set-value! a 4 'user)
(set-value! a 3 'user)
(forget-value! a 'user)

;; 由于 multipler 的 process-new-value 的触发至少需要有两个值存在 value
;; 我们只是设置 b 的话，系统无法知道两个 a 其实是同一个, 从而不会自动开根
(set-value! b 4 'user)
(has-value? a)
