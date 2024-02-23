#lang sicp

(#%require "./definition.rkt")
(#%require "../evaln.rkt")
(#%require "../env.rkt")

(define (analyze-definition exp)
  (let ([var (definition-variable exp)]
        [vproc (analyze (definition-value exp))])
    (lambda (env) (define-variable! var (vproc env) env))))

(#%provide analyze-definition)
