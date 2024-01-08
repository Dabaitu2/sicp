#lang racket

;; 这里的 withdraw 是简化版本，因为我们只是要讨论内部状态有什么问题
;; 使用内部状态
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 25))
(W 20)
(W 10)

;; 不使用内部状态的纯函数
(define (make-decrementer balance)
  (lambda (amount) (- balance amount)))

(define D (make-decrementer 25))
(D 20)
(D 10)

;; 对于 make-decrementer
;; 我们可以使用代换模型如下
;; (D 20) -> ((make-decrementer 25) 20) -> ((lambda (amount) (- 25 amount)) 20) -> 5
;;
;; 然而同样的情况却无法用在 make-simplified-withdraw
;; (W 20) -> ((make-simplified-withdraw 25) 20) -> ((lambda (amount) (set! balance (- 25 amount)) 25) 20)
;; -> (set! (balance (- 25 20))) 25) -> 25
;; 这是不符合实际意义的(因为 set 已经将后一个 balance 的值改变了，此处不能直接代换成 25),
;; 这意味着代换模型无法用在具有赋值情况的过程中
;; 同引号的引入破坏了 "值可以直接相互代换" (转变为在不同的上下文下可能有不同的表现) 的设想一样
;; 这两者都打破了 “referentially transparent" 这一性质

;; set! 或者说赋值的引入带来了另一个深远问题
;; 我们如何确定在模拟的世界中的两个 object 是 **相同** 的?
;; 本身现实;a世界中如何两个东西 "相同" 就是个很模糊的概念了
;; 我们通常只能用 “观察法" 来判断, 也就是说，如果我们对被观察的两个东西中的其中一个做了改变。
;; 在另外一个东西上也能看到相同的变化，那么我们可以认为它们是相同的。
;; 然而这又引入了一个近乎套娃的问题: 我们得先有一个 先验概念，知道我们要观察的东西预先觉得是 "同一" 的
;; 才能进一步的定义发生在 ”同一“ 事物上的变化，啊，philosophy , 先有鸡还是先有🥚？

;; 不过，对于 sameness 和 change 的讨论建立在 对象是可变的 的基础上, 如果对象不可变
;; 那么完全可以将一个复合对象看作其片段组成的整体。比如, 我们通常不会将一个 有理数 对象的 denom / numer 设置成可变的
;; 那么 “变化” 这件事就不会出现在这个有理数片段上，自然也就不用去讨论变化后的有理数是不是同一个了

;; 广泛采用 assignment 的程序设计被称为 命令式程序设计
;; 除了会导致 可计算模型 (computational model) 上的复杂度以外，命令式程序还很容易出现一些在函数是程序中不会出现的错误
;; 例如 下面的 迭代求阶乘程序

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))

;; 如下是命令式风格的求阶乘
;; 会发现程序的正确性开始·依赖于我们的命令顺序了，如果两个 set! 语句·颠倒了就不对了
;; 一旦我们的程序涉及到并发，那么问题可能会更加复杂, 并发中各个部分的执行顺序可能会变得的非常微妙
(define (factorial-directive n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

