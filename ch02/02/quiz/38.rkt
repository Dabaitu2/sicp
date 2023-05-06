#lang sicp

;; fold-right
;; 之所以叫 fold-right 是因为它始终将第一个元素和他 右边 的所有元素的递归结果进行 fold
;; 本质上他就是 accmulate, 由于我们需要从最后一个元素开始处理，所以必须使用递归
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

;; fold-left 自然就是把最后一个元素和他左边的所有元素的递归结果进行 fold
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
(fold-right / 1 (list 1 2 3))
;; 1/1=1   1/2=1/2   1/2/3 = 1/6
(fold-left / 1 (list 1 2 3))

(fold-right list nil (list 1 2 3)) ;; => (1 (2 (3 nil)))
(fold-left list nil (list 1 2 3))  ;; =>(((nil 1) 2) 3)


(#%provide fold-left fold-right)
