#lang sicp

(#%require "./pair-assignment.rkt")
(#%require "../evaln.rkt")

(define (analyze-pair-car-assignment exp)
  (let ([eproc (analyze (pair-assign-expression exp))]
        [vproc (analyze (pair-assign-value exp))])
    (lambda (env succeed fail)
      ;; 先检查 exp 有没有值
      (eproc
       env
       ;; 若有，检查 value 有没有值
       (lambda (expr fail2)
         (vproc env
                ;; 若有, 将 val set 到 exp 的 car
                (lambda (val fail3)
                  (let ([old-value (car exp)])
                    (set-car! expr val)
                    ;; 同时调用 succeed, 但是将 rollback 的能力也同时透出
                    ;; 如果 succeed 所对应的过程遭遇失败, 这个 lambda 会被调用
                    ;; 使得 fail3 调用的同时 数据可以被还原
                    (succeed 'ok
                             (lambda ()
                               ;; 若后续遇到失败
                               (set-car! expr old-value)
                               (fail3)))))
                fail2))
       fail))))

(define (analyze-pair-cdr-assignment exp)
  (let ([eproc (analyze (pair-assign-expression exp))]
        [vproc (analyze (pair-assign-value exp))])
    (lambda (env succeed fail)
      ;; 先检查 exp 有没有值
      (eproc
       env
       ;; 若有，检查 value 有没有值
       (lambda (expr fail2)
         (vproc env
                ;; 若有, 将 val set 到 exp 的 car
                (lambda (val fail3)
                  (let ([old-value (cdr exp)])
                    (set-cdr! expr val)
                    ;; 同时调用 succeed, 但是将 rollback 的能力也同时透出
                    ;; 如果 succeed 所对应的过程遭遇失败, 这个 lambda 会被调用
                    ;; 使得 fail3 调用的同时 数据可以被还原
                    (succeed 'ok
                             (lambda ()
                               ;; 若后续遇到失败
                               (set-cdr! expr old-value)
                               (fail3)))))
                fail2))
       fail))))

(#%provide analyze-pair-car-assignment
           analyze-pair-cdr-assignment)
