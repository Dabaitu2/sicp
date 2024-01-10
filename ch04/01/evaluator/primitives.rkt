#lang sicp

(define (self-evaluating? exp)
  (cond
    [(number? exp) true]
    [(string? exp) true]
    [else false]))

(define (variable? exp)
  (symbol? exp))


(#%provide self-evaluating? variable?)
