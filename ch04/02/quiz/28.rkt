#lang sicp

;; 假设我们有一个高阶函数
(define (run-it op-gen op-gen-param paramA paramB)
  ((op-gen op-gen-param) paramA paramB))

(define (actual-op-gen param)
  (if (> param 1) + -))

;; 假设这里不使用 actual-value 而是 eval, 则本身 (op-gen op-gen-param) 的结果作为 operator 
;; 在调用时就不会被实际解释，而会变成一个 thunk, apply 只能识别 primitive-procedure 和 compound-procedure
;; 并不认识 thunk，这里就会报错 Unknown procedure type
(run-it actual-op-gen 2 1 2)
