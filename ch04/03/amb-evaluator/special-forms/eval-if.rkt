#lang sicp

(#%require "./if.rkt")
(#%require "../predicate.rkt")
(#%require "../evaln.rkt")

(define (analyze-if exp)
  (let ([pproc (analyze (if-predicate exp))]
        [cproc (analyze (if-consequent exp))]
        [aproc (analyze (if-alternative exp))])
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation, accepet pred-value & another fail continuation
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; fail continuation 
             fail))))

(#%provide analyze-if)
