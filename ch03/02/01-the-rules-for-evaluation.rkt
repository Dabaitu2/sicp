#lang racket

;; 在 environmen model of evaluation 中，一个过程总是由一些代码和一个指向环境的指针构成的 pair
;; 对于过程的定义本质上是求值一个 lambda 表达式并将其 bind 到一个 variable
(define (square x)
  (* x x))

(define square-actually
  (lambda (x) (* x x)))
