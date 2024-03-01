#lang sicp

(#%require "../utils.rkt")
(#%require "./add-or.rkt")
(#%require "./cond.rkt")
(#%require "./while.rkt")
(#%require "./eval-let.rkt")

(define (install-derived-form-package)
  (put eval-cond 'exp 'cond)
  (put eval-and 'exp 'and)
  (put eval-or 'exp 'or)
  (put eval-let 'exp 'let)
  (put eval-let* 'exp 'let*)
  (put eval-letrec 'exp 'letrec)
  (put eval-while 'exp 'while)
  "derived form loaded")

(#%provide install-derived-form-package)
