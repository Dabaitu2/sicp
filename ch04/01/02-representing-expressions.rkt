#lang sicp

;; 使用数据抽象技术(抽象屏障), 分离操作规则和定义细节之间的联系
;; 也就是封装细节到 api 里，只暴露 api 给上层
;; 这里面的表达式全部都是利用list首个元素的特殊 tag 来表现的
;; 和 2.4 里面实现数据计算 package 的思路非常相似
(define (self-evaluating? exp)
  (cond
    [(number? exp) true]
    [(string? exp) true]
    [else false]))

(define (variable? exp)
  (symbol? exp))

;; 判断一个表达式是不是引号表达式其实就是看一个 list 的开头是不是某个特定符号
;; 这也是 lisp 比较优雅的地方，语法结构非常简单
;; 所有的东西其实都可以看作是 pair (cons) 构成的, 而不像其他语言那样要针对不同的语法结构写复杂的 parser
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp) (eq? (car exp) tag) false))

;; 处理赋值表达式
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

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
      (mke-lambda (cdadr exp) (cddr exp))))

;; 处理 lambda 表达式
(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

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

;; 处理对过程的应用 (即 (add 1 2) 之类的表达式, 有可能参数也是表达式)
(define (application? exp)
  (pair? exp))
(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))
(define (no-operands? ops)
  (null? ops))
(define (first-operand ops)
  (car ops))
(define (rest-operands ops)
  (cdr ops))

;; 派生表达式 Derived Expression
;; 基于其他特殊形式的表达式定义出来的特殊形式，不用直接去实现
;; 例如 cond 就可以看成嵌套执行 if
(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp)
  (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      `false
      (let ([first (car clauses)] [rest (cdr clauses)])
        (if (cond-else-clause? first)
            ;; 基准情形，分析到了 else，且 else 理应是最后一个 clause, 如果不是就会抛错
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            ;; 递归创建
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

