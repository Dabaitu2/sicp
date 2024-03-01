#lang sicp

(define (try a b)
  (if (= a 0) 1 b))

;; 这个代码在 scheme 中会报错，这是因为 scheme 是 “应用序”
;; 这导致参数会先被求值，而后面的 / 1 0 会导致除以 0 的错误
(try 0 (/ 1 0))

;; 如果能够让我们的语言实现规范序，那么下面的代码就会很有价值
;; 因为即使我们提供的某些参数本身可能会产出错误，但由于它们不被预先求值
;; 也可以保证逻辑的正常运行，就像是现代语言中常见的 try-catch 语句一样
(define (unless condition
          usual-value
          exceptional-value)
  (if condition exceptional-value usual-value))

(unless (= b 0)
  (/ a b)
  (begin
    (display "exceptional: returning 0")
    0))

;; 如果在参数进入 body 前需要被求值，那么我们认为这个过程对于参数是 “严格(strict)” 的
;; 反之则是 non-strict 的。

;; 在纯的应用序语言中，所有的参数都是 strict 的
;; 在纯的规范序语言中，
;;   1. 所有的复合过程参数都是 non-strict 的
;;   2. 对于基本 primitive 过程，其参数可能是 non-strict 也可能是 strict 的 
;; 还有语言甚至可以支持在参数中增加一些控制能力, 从而使得参数可以自行决定是不是 strict 的

;; 如果我们可以在构造数据结构的时候将过程作为 non-strict 的，比如对于 cons 我们这么做
;; 那么 我们就可以在还不知道元素的具体值的情况下 去完成一些有用的计算 例如获取 list 的长度

