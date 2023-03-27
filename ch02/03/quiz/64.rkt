#lang racket

;; helpers
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))



;; 下面过程将一个 有序表 变换成一颗平衡二叉树
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;; 以一个整数 n 和一个至少包含 n 个元素的表为参数，构造出一颗包含这个表的前 n 个元素
;; 的平衡树.
;; 返回一个 cons, car 是构造出的树, cdr 为没有包含在树中那些元素的 list
;; 非常容易可以看出用递归解决了问题，子问题就是拆解左右中，再 make tree 合并
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))


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

(tree->list-2 (list->tree (list 1 3 5 7 9 11)))

;; 可以看出还是树形结构，满足 T(n) = 2T(n/2) + O(1)
;; 由主定理推出为增长阶为 O(n * lgn)
