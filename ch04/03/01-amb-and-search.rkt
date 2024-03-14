#lang sicp
(#%require "../../common/math/num-theory.rkt")

;; 我们想要实现的这个求值器称为 amb evaluator (ambiguous evaluator)
;;;
;;; Amb-Eval input:
;;; (prime-sum-pair '(1 3 5 8) '(20 35 110)) ;;; Starting a new problem
;;; ;;; Amb-Eval value:
;;; (3 20)
;;;
;;;
;; (amb ⟨e1⟩ ⟨e2⟩ . . . ⟨en⟩)
;; 会不明确的返回 e1..en 中的某一个值 ei
;; 而再之后如果调用 (amb) 代表 "计算失败"
;; 这意味着这个世界失败了 因为amb 没有选择到任何的一个值
;; 那么求值器将再推一个数据
;;
;; 例如
;; 上面这个可能会产生出如下 6 个可能的值
;; (1 a) (1 b) (2 a) (2 b) (3 a) (3 b)

(display "Test list amb")
(newline)
;; 1. 产出 (1 'a)
(list (amb 1 2 3) (amb 'a 'b))

;; 相当于 (1 'a) 被判定失败，求值器重新推一个 (1' b)
(amb)
;; (1 'b) 失败，求值器再推一个 (2 'a)
(amb)
(amb)
(amb)
(amb)
;; 直到穷尽失败

;; 借助 (amb) 求值的这两个基本概念，我们可以进一步构造出 require 这个过程
;; 如果 p 不为真，我们就输出 (amb) 来表示求值失败
(define (require p)
  (if (not p) (amb)))

;; 基于 require 和 amb 求值器，我们可以构造出一个过程 an-element-of
;; 每一次 amb 的调用，可以看作是开启了新世界的分支
;; 例如，我们调用外界的 (amb (car items) ...)
;; 产出了第一个分支，也就是在 (car items) 和 (an-element-of) 里选择一个数的分支
;; 在解析内部的 an-element-of 时，再一个分支被创建
;; 当 items 被 cdr 穷尽为 null 时, require 会执行 (amb) 调用
;; 标志这一个分支走到了 dead-end, 此时！
;; 外界的那个 amb 就会尝试另一个值，因此另一个值的结果(car items) 会被外界的 an-element-of 返回
;; 交给再外界的 an-element-of (如果有) 进行选择
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (prime-sum-pair list1 list2)
  (let ([a (an-element-of list1)] [b (an-element-of list2)])
    (require (prime? (+ a b)))
    (list a b)))

;; 我们每一次调用 (amb) 都是在拒绝和当前的 amb 层级相同的那个世界返回的值，
;; 这里面的过程就会尝试去重新找到一个值，等待我们使用它或拒绝他
(display "Test an-element-of")
(newline)
(an-element-of '(1 2 3))
(amb)
(amb)
(display "Test prime-sum-pair")
(newline)
(prime-sum-pair '(1 2 3 4 5 6 7 8) '(1 2 3 4))
(amb)
(amb)
(amb)

;; 使用 (amb) 本质上也是一种类似惰性求值的东西
;; 在下面，我们实现了一个产出 > n 的所有整数的 amb
;; 每一个 (an-integer-starting-from) 都会产出 2 个值 (1 个固定的，还有一个lazy的)
;; 我们每次 (amb) 都会拒绝第一个，然后导致递归的调用第二个再产出两个值，从而可以再从其中选择一个
;; 不过和流相比，amb 返回的就是一个值，而不是一个流对象
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;; 本质上，amb 所做的事情就是在搜索
;; 一个 amb 代表了一个  nondeterministic choice point (非确定性选择点)
;; 那么对于 amb 有两种使用方法：
;; 1. 假设我们有无穷的机器，无穷的并发能力，每当遇到 amb，我们都会分配更多的机器去处理可能产生的值，或者遭遇成功/结束

;; 2. 如果 (大部分情况下) 我们的只有一台机器，只能执行一个或有限个并发进程
;; 我们就必须实现成顺序的方式。是指再遇到一个选择点的时候随机选取分支继续。
;; 而一旦这样的搜索遇到了失败, 我们就必须重新运行求值器（不是 amb，是整个求值器)，再随机的选择
;; 但这样肯定就是肉眼可见的低效了。因此我们可能需要一种系统化的搜索所有的可能执行路径。
;; 我们本节要实现的系统化搜索方式，就是
;; a. 每次遇到 (amb) 时，默认选择第一个可能性。
;; b. 一旦遇到失败，求值器可以自动 magically 回溯到最近的选择点去实验下一个可能性
;; c. 如果在任何选择点用完了所有可能性，就退出到上一层选择点，从那里继续
;; 这种策略称为 depth-first-search 或者 chronological backtracking （按时间回溯)


