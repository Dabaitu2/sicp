#lang sicp

;; 处理表达式列表
(define (first-exp seq)
  (car seq))
(define (rest-exps seq)
  (cdr seq))
(define (no-exps seq)
  (null? seq))
(define (last-exp? seq)
  (null? (cdr seq)))

(#%provide first-exp no-exps rest-exps last-exp?)
