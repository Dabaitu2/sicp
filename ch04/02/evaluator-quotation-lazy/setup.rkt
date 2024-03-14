#lang sicp

(#%require "./env.rkt")
(#%require "./procedure.rkt")
(#%require "./special-forms/lazy-list.rkt")
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
    ;; 还有一种更简单的写法，来自 https://www.inchmeal.io/sicp/ch-4/ex-4.33.html
    ;; 直接使用 actual-value 去预先求值我们支持的表达式
    ;; 从而轻松地注入全局函数
    (actual-value
     '(begin
        (define (cons x y)
          ;; Cool! 这就是类似于 JS 中的 [native code]
          ;; 注意，这里产生的就是解释器内部所使用的 cons
          ;; 但是我们要通过我们注入的 underlying-car / cdr 才能访问
          (underlying-cons 'lazy-cons (lambda (m) (m x y))))
        ;; 这里接收到的 z 就是一个 underlying-cons
        ;; 用户一般的 car / cons 其实是要加一层胶水才能访问的
        (define (car z)
          ((underlying-cdr z) (lambda (p q) p)))
        (define (cdr z)
          ((underlying-cdr z) (lambda (p q) q))))
     initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    ;; (define stime (runtime))
    (let ([output (actual-value input
                                the-global-environment)])
      ;; (newline)
      ;; (display (list "Time Taken: " (- (runtime) stime)))
      ;; (newline)
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
  (cond
    [(compound-procedure? object)
     (display (list 'compund-procedure
                    (procedure-parameters object)
                    (procedure-body object)
                    '<procedure-env>))]
    [(lazy-list? object) (print-lazy-list object)]
    [else (display object)]))

(#%provide driver-loop)
