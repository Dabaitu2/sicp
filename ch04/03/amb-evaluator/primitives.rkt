#lang sicp

(#%require "./env.rkt")

(define (self-evaluating? exp)
  (cond
    [(number? exp) true]
    [(string? exp) true]
    [else false]))

(define (variable? exp)
  (symbol? exp))

;; 执行 self-evaluating 本身是不会失败的
;; 失败一定是来自于后续 succeed 过程传播上来的
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail) (succeed exp fail)))

;; 执行 lookup-variable-value 本身是不会失败的
;; 失败一定来自于后续 succeed 过程传播上来的
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(#%provide self-evaluating?
           variable?
           analyze-self-evaluating
           analyze-variable)
