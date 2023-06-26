#lang racket

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)

;; 计算方式
;; 1. 过程创建, square, sum-of-squares f 的 bindings 都被放入全局环境的 frame 中
;; 2. 求值 (f 5) 就是将 5 应用到 f 对应的过程对象 上, 创建一个新环境，创建 a -> 5 的 binding，并求值 f 对应的 body 表达式
;; 3. 求值 (sum-of-squares (+ a 1) (* a 2)), 这又是一个复合过程, 我们优先求解子表达式 （+ a 1) (* a 2) 得到结果 6， 10。
;;    将 6.10 应用到 sum-of-squares, 开始重复 2 的流程， 创建一个新环境， 创建 x, y -> 6,10 的 binding， 求解 sum-of-squares 的 body 表达式
;; 4. 求值 (+ (square 6) (square 10)) 还是需要先求值子表达式, 即 (square 6) 和 (square 10)
;; 5. 为 (square 6) 和 (square 10) 分别创建环境, 并应用过程,  绑定实参, 求值 body, 得到结果 36, 100
;; 回到 4. 36 + 100 = 136, 计算结束
