#lang sicp

;; 当前的 '() 所返回的 list 依然是有底层的 list 所构成的非延时表
;; 想要让我们自己定义的 car 能够支持这个表, 我们最好在底层将 car / cons / list 的实现进行替换
;; 当前这个问题需要我们在处理引号时，将 list 处理成惰性的表
;; 完整代码参见 evaluator-quotation-lazy
(car '(1 2 3))


;; 同时我们希望能够将这个表结构以合理的方式打印出来
;; 我们采取的策略是，让 cons 返回的实际上是一个解释器内部认识的结构 (underlying-cons)
;; 包装这个一般过程，并且携带一个内部才能正常读取的 tag lazy-cons
;; 而不是直接返回一个一般过程
;; 利用解释器内部的 api (car, cdr) (包装为 underlying-car, underlying-cdr) 来处理它
;; 我们就能让外部 car ，cdr 正常的获取 cons 里定义函数
;; 同时，这个产生的内部语法结构，又可以被 user-print 识别
;; 从而被想办法打印
;;
;; 由于我们的数据实际上在 cons 时是被存储在了 cons 返回的过程外界的环境中
;; 也就是闭包，因此我们要利用内部的 api lookup-variable-value 和 procedure-environment
;; 去获取这个内部过程的外界环境，以及这个环境中的目标变量
;; 也就是我们 cons 函数指定的参数 x, y

;; 同时，我们需要把初始化 env 和 setup 许多 eval 方法放到一起
;; 而不是像以前那样分开了
