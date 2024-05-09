#lang sicp

(#%require "../utils.rkt")

;; 处理 if 表达式
(define (if-fail? exp)
  (tagged-list? exp 'if-fail))
(define (if-fail-consequent exp)
  (cadr exp))
(define (if-fail-alternative exp)
  (caddr exp))
(define (make-if-fail consequent alternative)
  (list 'if-fail consequent alternative))

(#%provide if-fail?
           if-fail-consequent
           if-fail-alternative
           make-if-fail)
