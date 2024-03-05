#lang sicp

(#%require "./definition.rkt")
(#%require "../evaln.rkt")
(#%require "../env.rkt")

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env))

(#%provide eval-definition)
