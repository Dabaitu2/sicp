#lang sicp

(#%require "../evaln.rkt")
(#%require "../special-forms/lambda.rkt")
(#%require "./let.rkt")

;; let->组合式
;; combination 就是 (list procedure parameters)
(define (let->combination exp)
  ;; 处理 命名 let
  (if (let-named-clause? exp)
      (let ([var (let-named-name exp)]
            [defs (let-named-clause exp)])
        (eval (make-let (list var defs) (let-body defs))))
      (let ([bindings (let-bindings exp)])
        (cons (make-lambda (let-vars bindings)
                           (let-body exp))
              (let-values bindings
                )))))
(define (eval-let exp env)
  (eval (let->combination (let-clauses exp)) env))

(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

(#%provide eval-let eval-let*)
