#lang racket

; 合并两个有序列表
(define (merge lst1 lst2)
  (cond
    [(null? lst1) lst2] ; 如果第一个列表为空，返回第二个列表
    [(null? lst2) lst1] ; 如果第二个列表为空，返回第一个列表
    [(< (car lst1) (car lst2)) ; 如果第一个列表的头部较小
     (cons (car lst1)
           (merge (cdr lst1) lst2))] ; 将它添加到结果中，并递归合并剩余部分
    [else ; 否则
     (cons (car lst2)
           (merge lst1 (cdr lst2)))])) ; 将第二个列表的头部添加到结果中

; 归并排序函数
(define (merge-sort lst)
  (if (< (length lst) 2)
      lst ; 如果列表长度小于2，则直接返回（已经是有序的）
      (let* ([mid (quotient (length lst) 2)] ; 否则计算中点
             [left-half (take lst mid)] ; 获取左半边
             [right-half (drop lst mid)]) ; 获取右半边
        ;; 递归地对左右两边进行归并排序，然后合并它们
        ;; 对左半边进行归并排序
        ;; 并发情况下可以使用(future ...)来加速过程,例如: `(future(merge-sort left-half))`
        ;; 不过需要引入相应库: `(require racket/future)`
        ;; 这里保持简单不使用future特性。
        ;; 使用递归进行排序:
        (merge (merge-sort left-half)
               ;; 对右半边进行归并排序:
               (merge-sort right-half)))))

; 测试代码：
(define unsorted-list '(4 3 1 5 2))
(define sorted-list (merge-sort unsorted-list))

sorted-list ; 输出: '(1 2 3 4 5)

