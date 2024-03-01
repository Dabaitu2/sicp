#lang sicp

(#%require "./env.rkt")
(#%require "./procedure.rkt")
(#%require "./evaln.rkt")

(define (setup-environment)
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

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (define stime (runtime))
    (let ([output (eval input the-global-environment)])
      (newline)
      (display (list "Time Taken: " (- (runtime) stime)))
      (newline)
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

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
