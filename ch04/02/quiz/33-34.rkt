#lang sicp

;; 当前的 '() 所返回的 list 依然是有底层的 list 所构成的非延时表
;; 想要让我们自己定义的 car 能够支持这个表, 我们最好在底层将 car / cons / list 的实现进行替换
;; 当前这个问题需要我们在处理引号时，将 list 处理成惰性的表
;; 同时我们希望能够将这个表结构以合理的方式打印出来
;; 完整代码参见 evaluator-quotation-lazy
(car '(1 2 3))


