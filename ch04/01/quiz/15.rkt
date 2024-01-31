#lang sicp

;; NP 问题的实例: 停机定理 
;; 这是一个不可计算问题, 有良好的定义，但无法实现

(define (run-forever) (run-forever))

;; 无法写出一个 halts? 函数去判断 p 针对参数 a 是否终止
;; 下面是一个实例
(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

;; 这样的调用，无论是什么结果 （终止或者永远运行）都将违背确定的 halts? 所定义的行为
(try try)


;; 假设 halts 判定 try 会终止, 也就是说 try 函数自身会停机
;; 则 run-forver 将永远运行，然而 halts 的判定却是终止, 逻辑矛盾
;; 假设 halts 判定 try 自身不会终止, 也就是 try 不会停机
;; 然而 try 会停止执行，因为它没有运行 run-forever, 这样的逻辑也是矛盾的


;; 固从逻辑上而言，这样一个 halts 将永远无法真实的判断一个过程是否会停止，这就是停机问题



