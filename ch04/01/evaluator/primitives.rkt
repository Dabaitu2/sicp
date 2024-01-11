#lang sicp

(define (self-evaluating? exp)
  (cond
    [(number? exp) true]
    [(string? exp) true]
    [else false]))

(define (variable? exp)
  (symbol? exp))


(define (lookup-variable-value exp env)
  (display "TODO"))

(#%provide self-evaluating? variable? lookup-variable-value)
