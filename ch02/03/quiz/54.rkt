#lang racket

;; 如果两个表包含着同样元素，这些元素也按同样顺序排列，那么就称这两个表为 equal ?
(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
      (eq? a b)))

(equal? `(this is a list) `(this is a list))
(equal? `(this is a list) `(this (is a) list))
