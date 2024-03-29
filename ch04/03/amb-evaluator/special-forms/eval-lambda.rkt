#lang sicp

(#%require "./lambda.rkt")
(#%require "./definition.rkt")
(#%require "../derived-forms/let.rkt")
(#%require "../evaln.rkt")
(#%require "../procedure.rkt")
(#%require
 "../../../../common/data/conventional-interface.rkt")

(define (scan-out-defines proc-body)
  (let ([result
         (fold-right
          (lambda (cur acc)
            (if (definition? cur)
                ;; 构造变量提升
                (let ([def-var
                       (list
                        (definition-variable cur)
                        ;; 变量会被 evaluated 一次，因此要多加一层 quote
                        ''*unassigned)]
                      ;; 构造变量赋值用于占位
                      [def-set
                       (list 'set!
                             (definition-variable cur)
                             (definition-value cur))])
                  ;; 这里没有把顺序倒回去，因为对于 binding / set! 定义而言这应该不重要
                  (list (cons def-var (car acc))
                        (cons def-set (cadr acc))))
                ;; 如果非内部定义，就直接原样放回去
                (list (car acc) (cons cur (cadr acc)))))
          (list '() '())
          proc-body)])
    (let ([bindings (car result)] [body (cadr result)])
      (if (null? bindings)
          body
          (list (make-let bindings body))))))

(define (analyze-lambda exp)
  (let ([vars (lambda-parameters exp)]
        [bproc (analyze-sequence
                (scan-out-defines (lambda-body exp)))])
    (lambda (env success fail)
      (success (make-procedure vars bproc env) fail))))

(#%provide analyze-lambda)
