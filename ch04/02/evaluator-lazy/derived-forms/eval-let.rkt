#lang sicp

(#%require "../evaln.rkt")
(#%require "../special-forms/lambda.rkt")
(#%require "./let.rkt")

;; let->组合式
;; combination 就是 (list procedure parameters)
(define (let->combination exp)
  ;; 处理 命名 let
  (if (let-named-clause? exp)
      (let ([name (let-named-name exp)]
            [clause (let-named-clause exp)])
        (let ([bindings (let-bindings clause)]
              [body (let-body clause)])
          (let ([vars (let-vars bindings)]
                [vals (let-values bindings
                        )])
            (let ([proc (make-lambda vars body)])
              (cons (make-lambda
                     '()
                     (list (cons 'define (list name proc))
                           (cons name vals)))
                    '())))))
      (let ([bindings (let-bindings exp)])
        (cons (make-lambda (let-vars bindings)
                           (let-body exp))
              (let-values bindings
                )))))
(define (eval-let exp env)
  (eval (let->combination (let-clauses exp)) env))

(define (eval-let* exp env)
  (eval (let*->nested-lets (let-clauses exp)) env))

(define (eval-letrec exp env)
  (eval (letrec->let exp) env))

(#%provide eval-let eval-let* eval-letrec)
