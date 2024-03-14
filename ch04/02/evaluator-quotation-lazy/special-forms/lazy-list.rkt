#lang sicp
(#%require "../utils.rkt")
(#%require "../env.rkt")
(#%require "../procedure.rkt")
(#%require "../evaln.rkt")

(define (lazy-list? exp)
  (tagged-list? exp 'lazy-cons))

;; 这里本身可以使用递归打印 + 迭代次数限制 去打印更多东西
;; 但我觉得对这道题而言这不是重点，故仅打印 CAR 即可
(define (print-lazy-list exp)
  (display "(")
  (display (force-it (lookup-variable-value
                      'x
                      (procedure-environment
                       (cdr exp)))))
  (display ". ")
  (display "lazy")
  (display ")"))

(#%provide lazy-list? print-lazy-list)
