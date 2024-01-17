#lang sicp

(#%require "./utils.rkt")

;; 处理过程
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (procedure-parameters p)
  (cadr p))
(define (procedure-body p)
  (caddr p))
(define (procedure-environment p)
  (cadddr p))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(#%provide make-procedure
           procedure-parameters
           procedure-environment
           procedure-body
           compound-procedure?)
