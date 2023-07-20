#lang racket

(define (make-withdraw initial-amount)
  (let ([balance initial-amount])
    (lambda (amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))))

;; (let ((<var> <exp>)) <body>)
;; 实质上是 ((lambda (<var>) <body>) <exp>) 的语法糖
;;
;;
;;
;;             ┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
;;             │                                                                                                 │
;;  global-env │  make-withdraw                                                            W1                    │
;;             │      │                                                                    │                     │
;;             │      │                                                                    │                     │
;;             │      │                                                                    │                     │
;;             └──────┼────▲────────────────▲──────────────────────────────────────────────┼─────────────────────┘
;;                    │    │                │                                              │
;;                    │    │              ┌─┴────────────────┐                             │
;;                    ▼    │              │initial-amount:100│                             │
;;             params: initial-amount     │                  │    ┌──────────────┐         │
;;             body:((lambda (balance)    └──────────▲───▲───┘    │ balance:100  │         │                ┌─────────────┐
;;                     xxxxxx)                       │   └────────│       -> 50  │ ◄───────┼────────────────┤ amount: 50  │
;;                     initial-amount)               │            └──────────────┘         │                │             │
;;                                                   │                           ▲         ▼                └─────────────┘
;;                                                  params: balance              │        params: amount
;;                                                  body: (lambda (amount)       └─────── body:(if (>= balance amount)
;;                                                          (if (>= balance amount)             (begin ...)
;;                                                          (begin ...)                         ...)
;;                                                          ...))
;;
;;
;;
;;
;; 首先 define 求值最外层 lambda 表达式，获得环境对象, 绑定到全局环境 make-withdraw
;; 然后调用 (define W1 (make-withdraw 100))
;; 先求解子表达式 (make-withdraw 100)
;;     1. 创建新环境 E1, 将 100 绑定到 initial-amount 形式参数
;;     2. 在新环境 E1 中求值 make-withdraw 对应的函数体
;;     3. 函数体又是个表达式，先求解子表达式 (lambda (balance xxx)), 再次产生环境对象，指向产生环境对象的环境上也就是 E1
;;     4. 应用 initial-amount 到环境对象，创建新环境 E2, 将 initial-amount 对应的值 100 绑定到 balance 形式参数
;;     5. 在新环境 E2 中求值环境对象对应的 body, body 还是一个 lambda 表达式，求解之，再获得一个环境对象，指向 E2
;;     6. 这次 (make-withdraw 100) 终于求解完毕, 将目标环境对象绑定到全局Env frame 的 W1 上
;; 调用 (W1 50) 创建新环境 E3, 指向 W1 指向的环境 E2。(其实也比较好理解，W1 之所以要指向 E2, 是因为我们需要能够让 W1 获得创建的新 binding 的值)
;; 然后在 E3 下求值 W1 的 body. 此时内部的 set! 将 balance 的 binding 改变为 50
;; (define W2) 和 1-6 完全一致,  但是创建的环境均是崭新的
;; 和原来的 make-withdraw 相比，使用 let 会多一层环境 + 一个环境对象，这是因为形式参数变成了一层额外的求值 lambda + 应用过程 


;; p.s: 到这里，lisp 大道至简的感觉有点出来了
;; 我们迄今为止写过的所有程序, 本质上都只是在操纵
;; 1. lambda 表达式 用于构造过程, lambda 将 lambda表达式自身看作一等公民 , 因此 lambda 中的任何部分都可以替换成 lambda
;; 2. define 将 value 确定到对应的环境中的 frame 的 binding
;; 3. 没了! let 只是 lambda 的语法糖, 加减乘除, 内置函数等只是预先定义的方法, 甚至组成数据的基本元素 cons 都是过程! 那么基于 cons 产生的一切
;; 也都只是组合和抽象的产物罢了

(define W1 (make-withdraw 100)) 
(W1 50)
(define W2 (make-withdraw 100))
