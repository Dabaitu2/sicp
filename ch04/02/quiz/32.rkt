#lang sicp

;; 对于 cons-stream, cons-stream 这种特殊形式 的 car 是 strict 的，因此会被立即求值
;; 而如果换成我们的 lazy-cons, 则不会有任何输出，因为所有参数都不会被求值
(define hi
  (cons-stream
   (begin
     (display "123")
     (newline)
     '123)
   (begin
     (display "456")
     (newline)
     '456)))

;; 这种 extra-lazyness
;; 比较适合用来求解存在递归定义的问题
;; 例如前面所说的求一阶常微分方程的问题


