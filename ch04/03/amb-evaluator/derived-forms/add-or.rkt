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
(define (analyze-and exp)
  (expand-and (and-clauses exp)))

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

(define (analyze-or exp)
  (analyze (or->if exp)))

(#%provide analyze-and analyze-or)
