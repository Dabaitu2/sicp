#lang sicp

(#%require "../utils.rkt")
(#%require "../sequence.rkt")
(#%require "../evaln.rkt")
(#%require "../special-forms/if.rkt")

(define (and? exp)
  (tagged-list? exp 'and))
(define (and-clauses exp)
  (cdr exp))
(define (expand-and exps)
  (if (no-exps exps)
      'true
      (make-if (first-exp exps)
               (expand-and (rest-exps exps))
               'false)))
(define (and->if exp)
  (expand-and (and-clauses exp)))
(define (eval-and exp env)
  (eval (and->if exp) env))

(define (or? exp)
  (tagged-list? exp 'or))
(define (or-clauses exp)
  (cdr exp))
(define (expand-or exps)
  (if (no-exps exps)
      'false
      (make-if (first-exp exps)
               'true
               (expand-or (rest-exps exps)))))
(define (or->if exp)
  (expand-or (or-clauses exp)))
(define (eval-or exp env)
  (eval (or->if exp) env))

(#%provide eval-and eval-or)
