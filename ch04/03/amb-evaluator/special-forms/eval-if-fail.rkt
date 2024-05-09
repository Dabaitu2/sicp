#lang sicp

(#%require "./if-fail.rkt")
(#%require "../evaln.rkt")

(define (analyze-if-fail exp)
  (let ([cproc (analyze (if-fail-consequent exp))]
        [aproc (analyze (if-fail-alternative exp))])
    (lambda (env succeed fail)
      (cproc env
             succeed
             (lambda () (aproc env succeed fail))))))

(#%provide analyze-if-fail)
