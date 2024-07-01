#lang sicp

(#%require "./require.rkt")
(#%require "../predicate.rkt")
(#%require "../evaln.rkt")

(define (analyze-require exp)
  (let ([pproc (analyze (require-predicate exp))])
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value))
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

(#%provide analyze-require)
