#lang racket

;; helpers
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
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond [(= x1 x2)
               (cons (x1 (intersection-set (cdr set1) (cdr set2))))]
              [(< x1 x2)
               (intersection-set (cdr set1) set2)]
              [(> x1 x2)
               (intersection-set set1 (cdr set2))]))))


;; 使用类似 element-of-set? 的方式实现 adjoin-set
(define (adjoin-set x set)
  (cond
    [(null? set) (list x)]
    [(= x (car set)) set]
    [(< x (car set)) (cons x set)]
    [else (cons (car set) (adjoin-set x (cdr set)))]))

;; O(n) 实现 union-set
(define (union-set set1 set2)
  (cond
    [(null? set1) set2]
    [(null? set2) set1]
    [(= (car set1) (car set2))
     (cons (car set1) (union-set (cdr set1) (cdr set2)))]
    [(< (car set1) (car set2))
     (cons (car set1) (union-set (cdr set1) set2))]
    [else (cons (car set2) (union-set set1 (cdr set2)))]))
