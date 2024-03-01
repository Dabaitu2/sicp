#lang sicp

(#%require "../utils.rkt")

;; 处理 if 表达式
(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
;; 这里返回的 'false, 我们会在全局环境中为其建立一个 binding?
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) (cadddr exp) 'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(#%provide if?
           if-predicate
           if-consequent
           if-alternative
           make-if)
