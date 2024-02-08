#lang sicp

(#%require "./assignment.rkt")
(#%require "../evaln.rkt")
(#%require "../env.rkt")

(define (analyze-assignment exp)
  (let ([var (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(#%provide analyze-assignment)
