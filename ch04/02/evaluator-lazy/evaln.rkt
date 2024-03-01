#lang sicp

(#%require "./utils.rkt")
(#%require "./env.rkt")
(#%require "./procedure.rkt")
(#%require "./primitives.rkt")
(#%require "./application.rkt")
(#%require "./sequence.rkt")

;; =============== lazy ==================
(define (lazy-param? p)
  (and (pair? p) (eq? (cadr p) 'lazy) (null? (cddr p))))

(define (lazy-memo-param? p)
  (and (pair? p)
       (eq? (cadr p) 'lazy-memo)
       (null? (cddr p))))

(define (lazy? obj)
  (tagged-list? obj 'lazy))

(define (lazy-memo? obj)
  (tagged-list? obj 'lazy-memo))

(define (evaluated-lazy-memo? obj)
  (tagged-list? obj 'evaluated-lazy-memo))

(define (transform-params parameters)
  (if (null? parameters)
      '()
      (let ([cur-param (first-exp parameters)]
            [rest-params (rest-exps parameters)])
        (cond
          [(or (lazy-param? cur-param)
               (lazy-memo-param? cur-param))
           (cons (car cur-param)
                 (transform-params rest-params))]
          [(variable? cur-param)
           (cons cur-param (transform-params rest-params))]
          [else
           (error "Bad Syntax -- TRANSFORM-PARAMS"
                  cur-param)]))))

;; work for both lazy & lazy-memo
(define (thunk-exp thunk)
  (cadr thunk))

(define (thunk-env thunk)
  (caddr thunk))

(define (thunk-value evaluated-lazy-memo)
  (cadr evaluated-lazy-memo))

(define (delay-lazy exp env)
  (list 'lazy exp env))

(define (delay-lazy-memo exp env)
  (list 'lazy-memo exp env))

(define (force-it obj)
  (cond
    [(lazy? obj)
     (actual-value (thunk-exp obj) (thunk-env obj))]
    [(lazy-memo? obj)
     (let ([result (actual-value (thunk-exp obj)
                                 (thunk-env obj))])
       (set-car! obj 'evaluated-lazy-memo)
       (set-car! (cdr obj) result)
       (set-cdr! (cdr obj) '())
       result)]
    [(evaluated-lazy-memo? obj) (thunk-value obj)]
    [else obj]))

(define (actual-value exp env)
  ;; 将 eval 出来的结果强制求值
  (force-it (eval exp env)))

;; 针对 primitive 求值
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

;; 如果是 (xx lazy) or (xx lazy-memo) 则转化为 lazy / lazy-memo thunk
;; 否则直接求值
;; 针对 delay， 转化为 thunk
(define (list-of-delayed-args params exps env)
  (if (no-operands? exps)
      '()
      (cons
       (cond
         [(lazy-param? (first-operand params))
          (delay-lazy (first-operand exps) env)]
         [(lazy-memo-param? (first-operand params))
          (delay-lazy-memo (first-operand exps) env)]
         [else (actual-value (first-operand exps) env)])
       (list-of-delayed-args (rest-operands params)
                             (rest-operands exps)
                             env))))

;; ============= lazy =====================
(define (eval-sequence exps env)
  (cond
    [(last-exp? exps) (eval (first-exp exps) env)]
    [else
     (eval (first-exp exps) env)
     (eval-sequence (rest-exps exps) env)]))

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
     (apply (actual-value (operator exp) env)
            (operands exp)
            env)]
    [else (error "Unknown expression type: EVAL" exp)]))

(define (apply procedure arguments env)
  (cond
    [(primitive-procedure? procedure)
     (apply-primitive-procedure
      procedure
      (list-of-arg-values arguments env))]
    [(compound-procedure? procedure)
     (let ([raw-parameters (procedure-parameters
                            procedure)])
       (eval-sequence
        (procedure-body procedure)
        (extend-environment
         (transform-params raw-parameters)
         (list-of-delayed-args raw-parameters arguments env)
         (procedure-environment procedure))))]
    [else
     error
     "Unknown procedure type -- APPLY"
     procedure]))

(#%provide eval apply eval-sequence actual-value)
