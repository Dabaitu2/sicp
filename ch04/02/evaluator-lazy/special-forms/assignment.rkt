#lang sicp
(#%require "../utils.rkt")

;; 处理赋值表达式
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))

(#%provide assignment? assignment-variable assignment-value)
