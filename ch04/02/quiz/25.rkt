#lang sicp

(define (unless condition
          usual-value
          exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

;; 在 application order 中使用这个 unless 会导致无限循环
;; 因为 unless 在调用时必须要求出每一个参数，从而导致 factorial 递归调用
(factorial 5)

;; 如果我们的过程是 normal order 的，那么这个过程会正常工作
;; 因为复合过程中只有参数被使用到了才会求值表达式，那么我们就可以避开提前调用 factorial
