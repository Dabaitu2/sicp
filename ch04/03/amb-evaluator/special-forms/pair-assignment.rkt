#lang sicp
(#%require "../utils.rkt")

(define (pair-assign-expression exp)
  (cadr exp))
(define (pair-assign-value exp)
  (caddr exp))
(define (pair-assign-car? exp)
  (tagged-list? exp 'set-car!))
(define (pair-assign-cdr? exp)
  (tagged-list? exp 'set-cdr!))

(#%provide pair-assign-expression
           pair-assign-value
           pair-assign-car?
           pair-assign-cdr?)
