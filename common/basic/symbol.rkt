#lang sicp

;; 如果两个表包含着同样元素，这些元素也按同样顺序排列，那么就称这两个表为 equal ?
(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (eq? (car a) (car b)) 
           (equal? (cdr a) (cdr b)))
      (eq? a b)))


(define nil '())

;; 以 一个符号和 一个表为参数。
;; 如果这个符号不包含在这个表里 (也就是说，它与表里的任何项 目都不eq?)，
;; memq 就返回假; 否则就返回该表的由这个符号的第一次出现开始的那个子表
(define (memq? item x)
  (cond
    [(null? x) false]
    [(eq? item (car x)) x]
    [else (memq? item (cdr x))]))
