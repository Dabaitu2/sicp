#lang sicp

;; 实现类似于求数组某一项的操作
;; 还是递归, 但是是尾递归，所以性能应该没问题
(define (list-ref items n)
  (if (= n 0) (car items) (list-ref (cdr items) (- n 1))))


;; 实现求数组长度的操作
;; null 是 scheme 提供的，用于检查参数是不是空 list
(define (length items)
  (if (null? items) 0 (+ 1 (length (cdr items)))))

;; 数组的append 操作
;; 组合两个 list
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

