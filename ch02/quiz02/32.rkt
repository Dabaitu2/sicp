#lang sicp

;; helpers
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))

;; subset 算法
;; 正序
;; from empty set []
;; [[], [1]]
;; [[], [1], [2], [1,2]]
;; [[], [1], [2], [1,2], [3], [1,3] [2,3] [1,2,3]]]
;; 倒序： 要解决 Subset s 的问题，演变成解决
;; 1. SubSet 去掉第一个元素，剩余的元素构成的子问题
;; 2. 问题 1 中的各个子集合 + 第一个元素构成的集合
;; 这两者的并集
;; 而很明显 1 这个子问题又可以进一步的细分，所以演变成递归问题
;; 我们之前常用的 bfs 只是这个解法的迭代解，但本质还是得看我们的递归解法
(define (subsets s)
  ;; 基准情况: 空集
  (if (null? s)
      (list nil)
      ;; 1 去掉当前元素的 子问题集合
      (let ([rest (subsets (cdr s))])
        (append rest
                ;; 2 子问题集合 + 当前元素
                (map (lambda (x)
                       (cons (car s) x)) rest)))))

(subsets (list 1 2 3))
(subsets (list 1 2 3 4))
