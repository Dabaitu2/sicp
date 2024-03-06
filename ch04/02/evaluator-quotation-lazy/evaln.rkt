#lang sicp

(#%require "./utils.rkt")
(#%require "./env.rkt")
(#%require "./procedure.rkt")
(#%require "./primitives.rkt")
(#%require "./application.rkt")
(#%require "./sequence.rkt")

;; ============ thunk ============
;; thunk 是用于实现惰性求值的数据结构
;; 它包括一个表达式和这个表达式对应的环境
(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk)
  (cadr thunk))

(define (thunk-env thunk)
  (caddr thunk))

;; 被记忆过的值，存储的被求出的值而非原始表达式
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

;; 获得实际的值, 在 normal order 中只有少许情况会使用它
(define (actual-value exp env)
  ;; (display "ready to force")
  ;; (newline)
  ;; (display (eval exp env))
  ;; (newline)
  ;; 将 eval 出来的结果强制求值
  (force-it (eval exp env)))
;; (unmemorized-force-it (eval exp env)))

(define (force-it obj)
  (cond
    [(thunk? obj)
     (let ([result (actual-value (thunk-exp obj)
                                 (thunk-env obj))])
       (set-car! obj 'evaluated-thunk)
       (set-car! (cdr obj) result)
       (set-cdr! (cdr obj) '())
       result)]
    [(evaluated-thunk? obj) (thunk-value obj)]
    [else obj]))

(define (unmemorized-force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; ============= eval 实现 =============

(define (eval-sequence exps env)
  (cond
    [(last-exp? exps) (eval (first-exp exps) env)]
    [else
     (eval (first-exp exps) env)
     (eval-sequence (rest-exps exps) env)]))

;; 用于求解过程应用 Procedure Application 的参数表
;; 以 combinations 的运算对象 operands 为参数，递归的求值并返回这些值的 list
;; (operand exp) -> exps
;; (define (list-of-values exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (cons (eval (first-operand exps) env)
;;             (list-of-values (rest-operands exps) env))))

;; 针对 primitive 求值
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

;; 针对 delay， 转化为 thunk
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

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
     ;; 更新 application 的处理, 我们会让 apply 去把参数变为 thunk
     ;; eval -> actual-value, 因为这里需要获取 operator 的真实值
     (apply (actual-value (operator exp) env)
            ;; 对于参数，则全部都是 delay 求值
            ;; remove list-of-values, 仅仅获取表达式本身
            (operands exp)
            ;; 增加第三个参数 env
            env)]
    [else (error "Unknown expression type: EVAL" exp)]))

;; 增加 env 参数
(define (apply procedure arguments env)
  (cond
    [(primitive-procedure? procedure)
     (apply-primitive-procedure
      procedure
      (list-of-arg-values arguments env))] ;; changed
    [(compound-procedure? procedure)
     (eval-sequence (procedure-body procedure)
                    (extend-environment
                     (procedure-parameters procedure)
                     (list-of-delayed-args arguments
                                           env) ;; changed
                     (procedure-environment procedure)))]
    [else
     error
     "Unknown procedure type -- APPLY"
     procedure]))

(#%provide eval apply eval-sequence actual-value force-it)
