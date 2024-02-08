#lang sicp

(#%require "./utils.rkt")
(#%require "./env.rkt")
(#%require "./procedure.rkt")
(#%require "./primitives.rkt")
(#%require "./application.rkt")

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env)
      (proc1 env)
      (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        ;; 递归嵌套函数执行
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ([procs (map analyze exps)])
    (if (null? procs) (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;; 对于过程的应用，我们现在不直接 apply (应用) 这个过程
;; 而也是转化为 仅仅执行语法分析过程, 生成待执行的结构
(define (analyze-application exp)
  (let ([fproc (analyze (operator exp))]
        [aprocs (map analyze (operands exp))])
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

;; 和 apply 略有差别
;; 不需要再 eval 一次过程了，因为过程已经被处理过 (也就是说 make-procedure 里面的 body 应该已经是被分析过了kk)
;; (解析这个过程的语法分析阶段做的，比如 (define) 或者 lambda)
;; 只需要创建一个新的环境，绑定对应参数即可
(define (execute-application proc args)
  (cond
    [(primitive-procedure? proc)
     (apply-primitive-procedure proc args)]
    [(compound-procedure? proc)
     ((procedure-body proc)
      (extend-environment (procedure-parameters proc)
                          args
                          (procedure-environment proc)))]
    [else
     (error "Unknown procedure type -- EXECUTE-APPLICATION"
            proc)]))

;; 用于求解过程应用 Procedure Application 的参数表
;; 以 combinations 的运算对象 operands 为参数，递归的求值并返回这些值的 list
;; (operand exp) -> exps
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond
    [(self-evaluating? exp) (analyze-self-evaluating exp)]
    [(variable? exp) (analyze-variable exp)]
    [(get 'exp (car exp)) ((get 'exp (car exp)) exp)]
    [(application? exp) (analyze-application exp)]
    [else
     (error "Unknown expression type -- ANALYZE" exp)]))

(#%provide eval
           analyze-application
           analyze-sequence
           analyze)
