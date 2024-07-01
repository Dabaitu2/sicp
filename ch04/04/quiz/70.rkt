#lang sicp


;; The purpose of using let bindings in 
;; add-rule! and add-assertion! operaions is:
;; 因为 stream 是惰性求值的，如果我们直接使用下面这种方式
;; 那么在真的要读数据的时候，会陷入无限的递归
;; 第一个元素永远都是当前这个 assertion 的第一个元素，而后面的元素
;; 依然还是这个 assertion
;; 而通过 let 重新创建一个binding 之后,
;; 原始 assertion stream 当前状态
;; 将会被复制
;; 从而使得第一个元素就是当时那个时刻的元素
;; 我们才可以保证正常的使用这个 assertion stream

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)
