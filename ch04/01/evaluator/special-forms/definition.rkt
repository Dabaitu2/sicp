#lang sicp
(#%require "./lambda.rkt")
(#%require "../utils.rkt")

;; 处理定义表达式
;; 定义表达式有两种
;; 1. 定义一般变量
;; 2. 定义过程, 本质上就是定义一个 lambda 表达式的语法糖
(define (definition? exp)
  (tagged-list? exp 'define))

;; 如果 cadr 是 symbol 意味着是一般变量定义
;; 否则说明是过程定义，需要再深入一层拿变量名
(define (definition-variable exp)
  (display exp)
  (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(#%provide definition? definition-value definition-variable)
