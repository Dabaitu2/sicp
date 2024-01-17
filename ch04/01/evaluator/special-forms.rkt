#lang sicp

;; Special Form 不应该使用 eval, 而是自行实现求值逻辑
(#%require "./utils.rkt")
(#%require "./env.rkt")
(#%require "./procedure.rkt")
(#%require "../../../common/data/table.rkt")

;; 判断一个表达式是不是引号表达式其实就是看一个 list 的开头是不是某个特定符号
;; 这也是 lisp 比较优雅的地方，语法结构非常简单
;; 所有的东西其实都可以看作是 pair (cons) 构成的, 而不像其他语言那样要针对不同的语法结构写复杂的 parser
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  (cadr exp))

;; 处理赋值表达式
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env))

;; 处理定义表达式
;; 定义表达式有两种
;; 1. 定义一般变量
;; 2. 定义过程, 本质上就是定义一个 lambda 表达式的语法糖
(define (definition? exp)
  (tagged-list? exp 'define))

;; 如果 cadr 是 symbol 意味着是一般变量定义
;; 否则说明是过程定义，需要再深入一层拿变量名
(define (definition-variable exp)
  (if (symbol? (cadr exp)) (cadr exp) (caddr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env))

;; 处理 if 表达式
(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
;; 这里返回的 'false, 我们会在全局环境中为其建立一个 binding?
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) (cadddr exp) 'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; 处理 begin 表达式
(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cadr exp))
(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq)
  (car seq))
(define (rest-exps seq)
  (cdr seq))
(define (eval-beigin exp env)
  (eval-sequence (begin-actions exp) env))

;; 将一个 sequence 变化为表达式
;; 如果只有一个表达式，则返回那个表达式
;; 否则就用 begin 连起来一个一个执行
;; 这个主要是用给 过程里的 body 使用的
;; 过程里的 body 里的表达式如果不止一个，那么在提取完 body 成为一个 seq 后
;; 实际上都是转化成 begin 来一个一个执行的
(define (sequence->exp seq)
  (cond
    [(null? seq) seq]
    [(last-exp? seq) (first-exp seq)]
    [else (make-begin seq)]))
(define (make-begin seq)
  (cons 'beigin seq))

;; 处理 lambda 表达式
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

(define (install-special-form-package)
  (put 'exp 'quote text-of-quotation)
  (put 'exp 'if eval-if)
  (put 'exp 'assignment eval-assignment)
  (put 'exp 'definition eval-definition)
  (put 'exp 'lambda eval-lambda)
  (put 'exp 'begin eval-beigin))

(#%provide install-special-form-package
           sequence->exp
           make-if
           make-lambda
           text-of-quotation
           lambda-parameters
           lambda-body
           begin-actions)
