#lang sicp
(#%require "../utils.rkt")
(#%require "../evaln.rkt")
(#%require "../sequence.rkt")

;; 处理 begin 表达式
(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cadr exp))
(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))

;; 将一个 sequence 变化为表达式
;; 如果只有一个表达式，则返回那个表达式
;; 否则就用 begin 连起来一个一个执行
;; 这个主要是用给 过程里的 body 使用的
;; 过程里的 body 里的表达式如果不止一个，那么在提取完 body 成为一个 seq 后
;; 实际上都是转化成 begin 来一个一个执行的
(define (sequence->exp seq)
  (cond
    [(null? seq) seq]
    [(last-exp? seq) (first-exp seq)]
    [else (make-begin seq)]))
(define (make-begin seq)
  (cons 'begin seq))

(#%provide begin? begin-actions eval-begin sequence->exp)
