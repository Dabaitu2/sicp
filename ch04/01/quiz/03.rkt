#lang sicp

(#%require "../../../common/data/table.rkt")

(#%require "../evaluator/special-forms.rkt")
(#%require "../evaluator/derived-forms.rkt")
(#%require "../evaluator/primitives.rkt")

;; 理论上，这些东西应该在各个数据自身注册
(put 'exp 'quote text-of-quotation)
(put 'exp 'assignment eval-assignment)
(put 'exp 'definition eval-definition)
(put 'exp 'if eval-if)
(put 'exp
     'lambda
     (lambda (exp env)
       (make-procedure (lambda-parameters exp)
                       (lambda-body exp)
                       env)))
(put 'exp
     'begin
     (lambda (exp env)
       (eval-sequence (begin-actions exp) env)))
(put 'exp
     'cond
     (lambda (exp env) (evaln (cond->if exp) env)))

;; 数据导向的 eval
;; 特殊 form 都是一个 (cons tag exp)
;; 这类特殊 form 的表达式解析器都用 put 'exp 注册到 table 里面
(define (evaln exp env)
  (cond
    [(self-evaluating? exp) exp]
    [(variable? exp) (lookup-variable-value exp env)]
    ;; 如果可以找到注册的特殊表达式，就执行其 eval 逻辑
    [(get 'exp (car exp)) ((get 'exp (car exp) exp env))]
    ;; application 不是 (cons tag xxx) 的特殊表达式, 需要单独处理
    [(application? exp)
     (apply (evaln (operator exp) env)
            (list-of-values (operand exp) env))]
    [else (error "Unknown expression type: EVAL" exp)]))
