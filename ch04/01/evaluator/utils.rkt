#lang sicp

(define (tagged-list? exp tag)
  (if (pair? exp) (eq? (car exp) tag) false))

;; 处理表达式列表
(define (first-exp seq)
  (car seq))
(define (rest-exps seq)
  (cdr seq))
(define (no-exps seq)
  (null? seq))
(define (eval-sequence exp env)
  (display "TODO"))

(#%provide tagged-list? first-exp no-exps rest-exps eval-sequence)
