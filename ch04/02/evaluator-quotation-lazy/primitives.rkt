#lang sicp
(#%require "./utils.rkt")

;; 实际上，要想做到这一步，本身还需要很多努力
;; 我们需要用类似正则的方案去判断什么是字符串，什么是数字
;; 而这里直接通过 lisp 自带的逻辑解释了
;; 这是因为我们实际上交给 解释器运行的东西已经相当于是通过 AST 解析之后的东西了
;; 比如传进来的 exp 如果是个数字，本质上应该是 {type: numebr, val: xxx} 这样一个结构
;; 在这里，我们相当于已经将所有东西都转化为了解析完毕的表达式，我们做的事情只有"求值"
;; 而没有语法分析过程
(define (self-evaluating? exp)
  (cond
    [(number? exp) true]
    [(string? exp) true]
    [else false]))

(define (variable? exp)
  (symbol? exp))

(#%provide self-evaluating? variable?)
