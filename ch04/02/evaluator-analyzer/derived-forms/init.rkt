#lang sicp

(#%require "../utils.rkt")
(#%require "./add-or.rkt")
(#%require "./cond.rkt")
(#%require "./while.rkt")
(#%require "./eval-let.rkt")
(#%require "./unless.rkt")

(define (install-derived-form-package)
  (put analyze-cond 'exp 'cond)
  (put analyze-and 'exp 'and)
  (put analyze-or 'exp 'or)
  (put analyze-let 'exp 'let)
  (put analyze-let* 'exp 'let*)
  (put analyze-letrec 'exp 'letrec)
  (put analyze-while 'exp 'while)
  (put analyze-unless 'exp 'unless)
  "derived form loaded")

(#%provide install-derived-form-package)
