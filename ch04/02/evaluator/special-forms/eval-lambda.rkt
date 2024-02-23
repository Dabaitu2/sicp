#lang sicp

(#%require "./lambda.rkt")
(#%require "../utils.rkt")
(#%require "../procedure.rkt")

(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

(#%provide eval-lambda)
