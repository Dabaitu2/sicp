#lang sicp

;; 迭代的 sum 函数
(define (sum term a next b)
  (define (iter a result)
    (if (= a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
