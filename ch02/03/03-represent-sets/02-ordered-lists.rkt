#lang sicp

;; 使用 ordered lists 表示以加速集合操作
;; 为了简化讨论，此处的元素都是数值
;; 只需要检查到某个元素 > 当前要检查的元素, 意味着这个数据不存在了
(define (element-of-set? x set)
  (cond
    [(null? set) false]
    [(= x (car set)) #t]
    [(< x (car set)) #f]
    [else (element-of-set? x (cdr set))]))

;; 求交集策略
;; 通过递归实现类似游标的策略
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      `()
      (let ([x1 (car set1)] [x2 (car set2)])
        (cond
          [(= x1 x2)
           (cons (x1 (intersection-set (cdr set1)
                                       (cdr set2))))]
          [(< x1 x2) (intersection-set (cdr set1) set2)]
          [(> x1 x2) (intersection-set set1 (cdr set2))]))))

(define (adjoin-set x set)
  (cond
    [(null? set) (list x)]
    [(= x (car set)) set]
    [(< x (car set)) (cons x set)]
    ;; 递归放到后面检查，也是用递归实现游标
    [else (cons (car set) (adjoin-set x (cdr set)))]))

;; O(n) 增长阶实现 union set
(define (union-set set1 set2)
  (cond
    [(null? set1) set2]
    [(null? set2) set1]
    [(= (car set1) (car set2))
     (cons (car set1) (union-set (cdr set1) (cdr set2)))]
    [(< (car set1) (car set2))
     (cons (car set1) (union-set (cdr set1) set2))]
    [else (cons (car set2) (union-set set1 (cdr set2)))]))
