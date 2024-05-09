#lang sicp

(#%require "../utils.rkt")

(define (require? exp)
  (tagged-list? exp 'require))
(define (require-predicate exp)
  (cadr exp))

(#%provide require? require-predicate)
