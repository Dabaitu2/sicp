#lang sicp

;; 考虑如下表达式
(let ([a 1])
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

;; 这个表达式按照我们正文的解释方法无法正常执行
;; 因为 b 的 set 操作也先于 a
;; 导致 a 还没有赋值就被使用


;; 为了使得 define 的顺序不再重要，我们可以如下设计:
;; TODO: finish this
