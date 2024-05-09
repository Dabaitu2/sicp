#lang sicp

(#%require "../utils.rkt")

;; 处理赋值表达式
(define (p-assignment? exp)
  (tagged-list? exp 'permanent-set!))
(define (p-assignment-variable exp)
  (cadr exp))
(define (p-assignment-value exp)
  (caddr exp))

(#%provide p-assignment?
           p-assignment-variable
           p-assignment-value)
