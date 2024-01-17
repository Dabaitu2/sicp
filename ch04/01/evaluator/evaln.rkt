#lang sicp

(#%require "../../../common/data/table.rkt")

(#%require "./utils.rkt")
(#%require "./env.rkt")
(#%require "./procedure.rkt")
(#%require "./primitives.rkt")

(#%require "./special-forms.rkt")
(#%require "./application.rkt")

;; 数据导向的 eval
;; 特殊 form 都是一个 (cons tag exp)
;; 这类特殊 form 的表达式解析器都用 put 'exp 注册到 table 里面
(define (eval exp env)
  (cond
    [(self-evaluating? exp) exp]
    [(variable? exp) (lookup-variable-value exp env)]
    ;; 如果可以找到注册的特殊表达式，就执行其 eval 逻辑
    [(get 'exp (car exp)) ((get 'exp (car exp) exp env))]
    ;; application 不是 (cons tag xxx) 的特殊表达式, 需要单独处理
    [(application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env))]
    [else (error "Unknown expression type: EVAL" exp)]))

(define (primitive-procedure? procedure)
  (display "TODO"))
(define (apply-primitive-procedure procedure arguments)
  (display "TODO"))

(define (apply procedure arguments)
  (cond
    [(primitive-procedure? procedure)
     (apply-primitive-procedure procedure arguments)]
    [(compound-procedure? procedure)
     (eval-sequence (procedure-body procedure)
                    (extend-environment
                     (procedure-parameters procedure)
                     arguments
                     (procedure-environment procedure)))]
    [else
     error
     "Unknown procedure type -- APPLY"
     procedure]))

(#%provide eval apply)
