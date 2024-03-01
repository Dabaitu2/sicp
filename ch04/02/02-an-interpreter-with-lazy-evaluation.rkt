#lang sicp

;; 本章节将复合过程改编为 Normal Order 也就是非严格的
;; 而对于基本过程则保持 Application Order 也就是说严格的

;; 基本策略为: 在 Apply 一个 Procedure 时，解释器会判断出那些参数需要 Lazy-Evaluation
;; 对于需要 Lazy 的参数，均不求值，而是将其包装为一个 Thunk
;; Thunk 包含这个参数对应的表达式，以及求值这个 Procedure Application 时所处的 Env

;; 同样，我们需要实现一个类似于第三章提到的 Force 操作来实现求值一个 Thunk
;; 调用 Force 的时机通常是我们需要获得这个值时才进行，这包括：
;; 1. 应用基本过程需要这个值
;; 2. 它将作为条件表达式的 predicate 谓词时
;; 3. 将它作为一个过程 procedure 去 Apply 时


;; 然后，我们选择将这个 thunk 实现为 memorized thunk, 也就是带缓存的，这的选择也和前面第三章所讲的一致
;; 可以减少很多无用的开销，不过，这样也带来了其他的问题


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


;; 对于基本过程，参数需要立即求值
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))


;; 对于复合过程，我们采用 non-strict 的延时求值策略
;; 将值转化为 thunk 列表
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

;; 在 apply 中，修改对应的部分
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
                     (list-of-delayed-args arguments env) ;; changed
                     (procedure-environment procedure)))]
    [else
     error
     "Unknown procedure type -- APPLY"
     procedure]))


;; 在 if 中，谓词对应的部分也需要立即求值
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


;; 为了实现 force-it 对应的功能，我们需要实现一个数据结构：thunk
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
  (cadr (evaluated-thunk)))

;; 获得实际的值, 在 normal order 中只有少许情况会使用它
(define (actual-value exp env)
  ;; 将 eval 出来的结果强制求值
  (force-it (eval exp env)))

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



