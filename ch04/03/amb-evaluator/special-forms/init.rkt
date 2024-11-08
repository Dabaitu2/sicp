#lang sicp
(#%require "../utils.rkt")
(#%require "./quoted.rkt")
(#%require "./eval-definition.rkt")
(#%require "./eval-lambda.rkt")
(#%require "./begin.rkt")
(#%require "./eval-if.rkt")
(#%require "./eval-if-fail.rkt")
(#%require "./unbound.rkt")
(#%require "./eval-assignment.rkt")
(#%require "./eval-pair-assignment.rkt")
(#%require "./eval-p-assignment.rkt")
(#%require "./eval-require.rkt")

(define (install-special-form-package)
  (put analyze-quoted 'exp 'quote)
  (put analyze-assignment 'exp 'set!)
  (put analyze-permanent-assignment 'exp 'permanent-set!)
  (put analyze-definition 'exp 'define)
  (put analyze-if 'exp 'if)
  (put analyze-if-fail 'exp 'if-fail)
  (put analyze-lambda 'exp 'lambda)
  (put analyze-begin 'exp 'begin)
  (put analyze-unbound! 'exp 'unbound)
  (put analyze-require 'exp 'require)
  (put analyze-pair-car-assignment 'exp 'set-car!)
  (put analyze-pair-cdr-assignment 'exp 'set-cdr!)
  "special-form loaded")

(#%provide install-special-form-package)