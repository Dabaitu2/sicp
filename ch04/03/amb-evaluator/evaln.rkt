#lang sicp

(#%require "./utils.rkt")
(#%require "./env.rkt")
(#%require "./amb.rkt")
(#%require "./procedure.rkt")
(#%require "./primitives.rkt")
(#%require "./application.rkt")

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail)
      (proc1 env
             ;; success continuation
             (lambda (proc1-value fail2)
               ;; success will handle proc2
               ;; 所以可以看到，程序的下一步执行本质上是由这些语法解释结构自身实现的
               ;; 而不是一个通用结构处理
               (proc2 env succeed fail2))
             ;; failure continuation
             fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        ;; 递归嵌套函数执行
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ([procs (map analyze exps)])
    (if (null? procs) (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ([fproc (analyze (operator exp))]
        [aprocs (map analyze (operands exp))])
    (lambda (env succeed fail)
      ;; 必须 (fproc env) 执行成功了 (获取 operator)
      ;; 再去使用产出的实际 proc 去执行
      (fproc
       env
       (lambda (proc fail2)
         ;; get-args 针对整个 arguments 构成的 list 求值
         ;; 而不是像以前一样用一个 map 一个一个的求值
         (get-args
          aprocs
          env
          (lambda (args fail3)
            (execute-application proc args succeed fail3))
          fail2))
       fail))))

;; get-args 通过在 success 中递归调用 get-args 去实现
;; 在递归的 get-args 中，success 会被一层一层的用 lambda 包起来
;; 最终的 succeed 会拼接所有 args, 并交给最外面的那个 succeed 去
;;
;; 当最外层的 succeed 由于 cdr aprocs 会变成 nil
;; 触发 (succeed '())
;; 然后 => (succeed-outer (cons arg-outer '())))
;;       => (succeed-outer-outer (cons arg-outer-outer (cons arg-outer '()))))
;; ... 最终将所有的 args 拼起来给最外层的 succeed (从而 execute-application)
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond
    [(primitive-procedure? proc)
     (succeed (apply-primitive-procedure proc args) fail)]
    [(compound-procedure? proc)
     ((procedure-body proc)
      (extend-environment (procedure-parameters proc)
                          args
                          (procedure-environment proc))
      succeed
      fail)]
    [else
     (error "Unknown procedure type -- EXECUTE-APPLICATION"
            proc)]))

;; amb 求值器增加了 success continuation 和 failure continuation
;; 并将其得到的执行过程 (execution procedure: 被解析了语法结构的 body)
;; 应用到给定的环境和 continuation 上
;; 一般的执行过程是 (lambda (env) ...)
;; 这个新的执行过程大概如此:
;; (lambda (env succeed fail)
;;   ;; succeed is (lambda (value fail) ...)
;;   ;; fail is (lambda () ..)
;;   ...
;;  )
(define (ambeval exp env success fail)
  ((analyze exp) env success fail))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (ambeval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (analyze-amb exp)
  ;; 解析所有 amb 的可能值
  (let ([cprocs (map analyze (amb-choices exp))])
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ;; 尝试执行某一个可能值的求值
            ((car choices)
             env
             ;; 在这里，只要 (choice env 没有什么幺蛾子(里面主动抛出 (amb))，应该是会成功的)
             succeed
             ;; 否则就调用 fail, 继续尝试下一个可能值
             ;; 考虑一个情况:
             ;; (let ((a (amb 1 2 3)))
             ;;   (if (= a 3)
             ;;       1
             ;;       (amb)))
             ;; 我们通常遇到的 (amb) 触发的失败, 假设前面有 (amb 1 2 3) 这样的产出值，为什么还能触发 try-next 呢？
             ;; 这样是因为 fail 会被传给 succeed 的第二个参数中，当这个 succeed 触发后续逻辑失败 (也就是 if 里的 amb)
             ;; 那么这个 fail 就会被调用, 从而触发 try-next, 这就是核心所在
             ;; 我们只需要考虑第一次的 amb 的 succeed 是什么就好了, 而这将通过 driver loop 传入
             (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze exp)
  (cond
    [(self-evaluating? exp) (analyze-self-evaluating exp)]
    [(variable? exp) (analyze-variable exp)]
    ;; amb 是比特殊语法更特殊的表达式，先放在外面
    [(amb? exp) (analyze-amb exp)]
    [(get 'exp (car exp)) ((get 'exp (car exp)) exp)]
    [(application? exp) (analyze-application exp)]
    [else
     (error "Unknown expression type -- ANALYZE" exp)]))

(#%provide ambeval
           analyze-application
           analyze-sequence
           analyze)
