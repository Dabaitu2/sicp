#lang sicp

(#%require "./if.rkt")
(#%require "../predicate.rkt")
(#%require "../evaln.rkt")

(define (analyze-if exp)
  (let ([pproc (analyze (if-predicate exp))]
        [cproc (analyze (if-consequent exp))]
        [aproc (analyze (if-alternative exp))])
    (lambda (env)
      (if (true? (pproc env)) (cproc env) (aproc env)))))

(#%provide analyze-if)
