#lang racket

;; helpers
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

;; 前序遍历
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond
    [(null? set) #f]
    [(= x (entry set)) #t]
    [(< x (entry set))
     (element-of-set? x (left-branch set))]
    [(> x (entry set))
     (element-of-set? x (right-branch set))]))

(define (adjoin-set x set)
  (cond
    [(null? set) (make-tree x '() '())]
    [(= x (entry set)) set]
    [(< x (entry set))
     (make-tree (entry set)
                (adjoin-set x (left-branch set))
                (right-branch set))]
    [(> x (entry set))
     (make-tree (entry set)
                (left-branch set)
                (adjoin-set x (right-branch set)))]))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define test-tree
  (make-tree
   2
   (make-tree 3 (make-tree 4 '() '()) (make-tree 5 '() '()))
   (make-tree 6
              (make-tree 7 '() '())
              (make-tree 8 '() '()))))

;; 由于都是 in-order traversal, 两者对于各个类型的树具有相同的返回值
(tree->list-1 test-tree)
(tree->list-2 test-tree)

;; 分析两者发现均为树形递归，也就是先一步一步下去再一步一步汇总回来，且包含多个分叉
;; 所以两者本质上是同类的算法，然而由于 tree->list-1 中的 append 操作是 O(n) 的线性操作 (参见 data/sequence.rkt 的实现)
;; 因此我们可以利用主定理 master method 进一步理清两者的时间复杂度
;; list-1 可以表示为 T(n) = 2(T/n) + O(n)
;; 设 C = log{_2}2 = 1, 而 O(n) = ϴ(n^c), 故 T(n) = ϴ(n^clgn) = ϴ(n * lgn)
;; 对于 list-2, cons 是 O(1) < O(n^c) = O(1) 的操作，所以根据主定理 T(n) = O(n^c) = O(n)
;; 故在实际代码中，tree->list-2 效率会更好一些
