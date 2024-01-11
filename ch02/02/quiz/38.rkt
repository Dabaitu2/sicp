#lang sicp

;; fold-right 从 right 向 left 折叠, 最后折叠的元素是最左边的元素
;; 就像折纸，从最右边开始向最左边折, 因此第一个执行的是最右边的元素
;; f(1, f(2, f(3, z))), z = 1
;; 本质上他就是 accmulate, 由于我们需要从最后一个元素开始处理，所以必须使用递归
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

;; fold-left 则是从左向右折叠
;; 由于从第一个元素就开始处理，所以可以使用迭代
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


;; 实现 accumulate (reduce), 此乃线性递归
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; 只有同时满足结合率和交换律的 op 才可以满足 fold-left 与 fold-right 结果一致
;; 3/1 = 3  2/3 = 2/3 1/(2/3) = 3/2
;; f(1, f(2, f(3, z))), z = 1
(fold-right / 1 (list 1 2 3))
;; 1/1=1   1/2=1/2   1/2/3 = 1/6
(fold-left / 1 (list 1 2 3))

(fold-right list nil (list 1 2 3)) ;; => (1 (2 (3 nil)))
(fold-left list nil (list 1 2 3))  ;; =>(((nil 1) 2) 3)


(#%provide fold-left fold-right)
