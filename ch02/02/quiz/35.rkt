#lang sicp

(#%require "../../common/data/conventional-interface.rkt")

;; 用 accumulate 和 map 来表示 count-leaves
(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (subtree)
          (if (pair? subtree)
              ;; 利用递归处理子问题，先把子树的结果算出来，一条 list 变成一个数，
              ;; 然后最终处理一个最简单的 list
              (count-leaves subtree)
              1))
        t)))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
