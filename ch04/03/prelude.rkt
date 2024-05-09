#lang sicp
(#%require "../../common/math/num-theory.rkt")

;; 这里给出的就是一种被称为 non-determistic computing 非确定性计算的描述
;; 之所以叫非确定性计算，是因为它没有给出应该如何真正的具体实现求解的方案
;; 而是给出了类似对于定义说明的翻译，或者说偏 pseudo 代码的东西
;; 本章的目标，就是要通过修改求值器以实现此能力
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (prime-sum-pair list1 list2)
  (let ([a (an-element-of list1)]
        [b (an-element-of list2)])
    (require (prime? (+ a b)))
    (list a b)))

;; 此处依赖的关键 idea 是
;; 在一个非确定性语言中，表达式可以有多于一个可能的值，
;; 例如 an-element-of 可能返回给定的 list 中的任何一个元素, 我们的求值七回自动从中选出一个可能值
;; 并且跟踪这个选择, 如果随后的 require 无法满足，求值器就会尝试其他的选择直到成功或者选择已经用光

;; 比较 非确定性求值 和 流处理 是很有指导意义的
;; 流处理中利用了惰性求值，设法将流处理使用惰性求值来将可能答案流的组装时间与实际流元素的生成时间解耦
;;    在这种求值其中，我们构造出一种幻觉，所有可能结果都以一种没有时间顺序的方式摆在面前。
;; 而非确定性求值, 表达式则代表着对一系列的可能世界的探索。每一个世界都是由一系列选择所决定的。
;; 一些世界可能会走入 dead end，但另一些则可能会保存着有用的值。
;;    在这种求值中，我们会构造出另一种幻觉（假象 illusion): 时间存在分支
;;    我们的程序中保存着所有可能的不同执行历史，再遇到一个 dead end 时，我们总可以回到以前的某个选择点
;;    并沿着另一个分支继续下去


;; 我们想要实现的这个求值器称为 amb evaluator (ambiguous evaluator)
;;;
;;; Amb-Eval input:
;;; (prime-sum-pair '(1 3 5 8) '(20 35 110)) ;;; Starting a new problem
;;; ;;; Amb-Eval value:
;;; (3 20)
