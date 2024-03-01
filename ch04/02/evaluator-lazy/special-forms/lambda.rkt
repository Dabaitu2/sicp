#lang sicp
(#%require "../utils.rkt")

;; 处理 lambda 表达式
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(#%provide lambda?
           lambda-parameters
           lambda-body
           make-lambda)
