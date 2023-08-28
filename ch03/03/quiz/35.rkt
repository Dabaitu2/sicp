#lang sicp

(#%require "../../../common/math/num-theory.rkt")
(#%require "../05-propagation-of-constraints/units.rkt")
(#%require "../05-propagation-of-constraints/connector.rkt")

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER"
                   (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond
      [(eq? request 'I-have-a-value) (process-new-value)]
      [(eq? request 'I-lost-my-value)
       (process-forget-value)]
      [else (error "Unknown request: SQUARER" request)]))
  (connect a me)
  (connect b me)
  me)

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
