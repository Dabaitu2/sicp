#lang sicp

(#%require "./utils.rkt")
(#%require "./env.rkt")
(#%require "./procedure.rkt")
(#%require "./primitives.rkt")
(#%require "./application.rkt")
(#%require "./sequence.rkt")

(define (eval-sequence exps env)
  (cond
    [(last-exp? exps) (eval (first-exp exps) env)]
    [else
     (eval (first-exp exps) env)
     (eval-sequence (rest-exps exps) env)]))

;; 用于求解过程应用 Procedure Application 的参数表
;; 以 combinations 的运算对象 operands 为参数，递归的求值并返回这些值的 list
;; (operand exp) -> exps
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; 数据导向的 eval
;; 特殊 form 都是一个 (cons tag exp)
;; 这类特殊 form 的表达式解析器都用 put 'exp 注册到 table 里面
(define (eval exp env)
  (cond
    [(self-evaluating? exp) exp]
    [(variable? exp) (lookup-variable-value exp env)]
    ;; 如果可以找到注册的特殊表达式，就执行其 eval 逻辑
    [(get 'exp (car exp)) ((get 'exp (car exp)) exp env)]
    ;; application 不是 (cons tag xxx) 的特殊表达式, 需要单独处理
    [(application? exp)
     (display "exp: ")
     (display exp)
     (newline)
     (display "operator: ")
     (display (operator exp))
     (newline)
     (display "operands ")
     (display (operands exp))
     (newline)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env))]
    [else (error "Unknown expression type: EVAL" exp)]))


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

(#%provide eval apply eval-sequence)
