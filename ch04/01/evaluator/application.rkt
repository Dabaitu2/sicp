#lang sicp

;; 处理对过程的应用 (即 (add 1 2) 之类的表达式, 有可能参数也是表达式)
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

;; 用于求解过程应用 Procedure Application 的参数表
;; 以 combinations 的运算对象 operands 为参数，递归的求值并返回这些值的 list
;; (operand exp) -> exps
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(#%provide operator
           operands
           no-operands?
           first-operand
           rest-operands
           application?
           list-of-values)
