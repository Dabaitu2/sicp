#lang sicp

;; lisp remainder
;; 通过 减法 + 递归实现
(define (remainder n d)
  (if (< n d) n (remainder (- n d) d)))


;; 利用减法和递归
;; 写出 controller 描述
(controller 
  test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (reg a))
  ;; rem-loop 将用来替换原来的 (assign t (op rem) (reg a) (reg b))      
  rem-loop
    (test (op <) (reg t) (reg b))
    (branch (label rem-done))
    (assign t (op -) (reg t) (reg b))
    (goto (label rem-loop))
  rem-done
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
  gcd-done)


