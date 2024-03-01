#lang sicp

(#%require "./assignment.rkt")
(#%require "../evaln.rkt")
(#%require "../env.rkt")

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env))

(#%provide eval-assignment)
