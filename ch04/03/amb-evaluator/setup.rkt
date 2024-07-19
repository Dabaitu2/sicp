#lang sicp

(#%require "./env.rkt")
(#%require "./procedure.rkt")
(#%require "./evaln.rkt")
(#%require "./special-forms/init.rkt")
(#%require "./derived-forms/init.rkt")

(define (setup-environment)
  (install-special-form-package)
  (install-derived-form-package)
  ;; 将基本过程注入到环境中
  (let ([initial-env (extend-environment
                      (primitive-procedure-names)
                      (primitive-procedure-objects)
                      the-empty-enviroment)])
    ;; 定义全局变量
    ;; 全局变量 'true 对应解释器中的真值
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'nil '() initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ([input (read)])
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     ;; next-alternative 本质上是内部的 fail
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       ;; next-alternative 会变成下一轮的 try-again, 在用户输入 try-again 时被触发
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       driver-loop))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;;There is not current problem")
     (driver-loop))))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compund-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(#%provide the-global-environment driver-loop)
