#lang sicp

(#%require "./if.rkt")
(#%require "../predicate.rkt")
(#%require "../evaln.rkt")

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(#%provide eval-if)
