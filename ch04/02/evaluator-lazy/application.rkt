#lang sicp

;; 处理对过程的"应用"
;; (即 (add 1 2) 之类的表达式, 有可能参数也是表达式)
(define (application? exp)
  (pair? exp))
(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))
(define (no-operands? ops)
  (null? ops))
(define (first-operand ops)
  (car ops))
(define (rest-operands ops)
  (cdr ops))

(#%provide operator
           operands
           no-operands?
           first-operand
           rest-operands
           application?)
