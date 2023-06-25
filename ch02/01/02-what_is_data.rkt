#lang sicp

;; 2.1.3 数据意味着什么？
;; 一般而言我们将数据定义为
;; 1 & 2 一组适当的 Selector 和 Constructor
;; 3 以及使这些过程成为一套合法表示所必须满足的一组特定条件
;; 下面展示了如何将 Pair 也看待成数据的

;; 这里出现了闭包！我们返回了一个过程，而这个过程中存储了数据
;; 怪不得说 js 从 lisp 中吸取了大量经验
;; 这种将过程作为对象传递的方式被称为 Message passing [消息传递]
(define (cons x y)
  (define (dispatch m)
    (cond
      [(= m 0) x]
      [(= m 1) y]
      [else (error "Argument not 0 or 1 -- CONS" m)]))
  (dispatch))

(define (car z)
  (z 0))
(define (cdr z)
  (z 1))
