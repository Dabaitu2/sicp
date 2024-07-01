#lang sicp

(#%require "./definition.rkt")
(#%require "../evaln.rkt")
(#%require "../env.rkt")

(define (analyze-definition exp)
  (let ([var (definition-variable exp)]
        [vproc (analyze (definition-value exp))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               ;; 调用 succeed 挺诡异的，为啥?
               (succeed 'ok fail2))
             fail))))

(#%provide analyze-definition)
