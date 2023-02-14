#lang sicp

(#%require "../conventional-interface.rkt")


;; 使用 flatmap 完成全排列问题
;; remove 就是反向 filter
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; permutations 算法也是一种 dp;
;; 要想解决 S 的子问题, 需要对于 S 中的每一个元素 x
;; 递归的生成 S-x 的所有排列的序列 (子问题)
;; 然后将这个元素 x 组合到序列的最前面
;; P([1, 2, 3])
;; 1 -> 1, P([2, 3])
;;      1, 2, P(3)
;;      1, 3, P(2)
;; 2 -> 2, P([1, 3])
;;      2, 1, P(3)
;;      2, 3, P(1)
;; 3 -> 3, P([1, 2])
;;      3, 1, P(2)
;;      3, 2, P(1)
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(permutations (list 1 2 3))
