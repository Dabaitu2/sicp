#lang sicp

(#%require "./assignment.rkt")
(#%require "../evaln.rkt")
(#%require "../env.rkt")

(define (analyze-assignment exp)
  (let ([var (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env succeed fail)
      (vproc
       env
       ;; success continuation
       (lambda (val fail2)
         (let ([old-value (lookup-variable-value var env)])
           (set-variable-value! var val env)
           ;; 执行后续操作
           (succeed
            'ok
            ;; 如果 succeed 所代表的后续执行过程失败，fail2-continuation 本身会被调用
            ;; 不过我们要把 fail2 包装一下，还原赋值操作
            (lambda ()
              (set-variable-value! var old-value env)
              (fail2)))))
       ;; fail continuation
       fail))))

(#%provide analyze-assignment)
