#lang sicp

;; selectors
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

;; constructor 前序遍历
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

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;; 以一个整数 n 和一个至少包含 n 个元素的表为参数，构造出一颗包含这个表的前 n 个元素
;; 的平衡树.
;; 返回一个 cons, car 是构造出的树, cdr 为没有包含在树中那些元素的 list
;; 非常容易可以看出用递归解决了问题，子问题就是拆解左右中，再 make tree 合并
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ([left-size (quotient (- n 1) 2)])
        (let ([left-result (partial-tree elts left-size)])
          (let ([left-tree (car left-result)]
                [non-left-elts (cdr left-result)]
                [right-size (- n (+ left-size 1))])
            (let ([this-entry (car non-left-elts)]
                  [right-result (partial-tree
                                 (cdr non-left-elts)
                                 right-size)])
              (let ([right-tree (car right-result)]
                    [remaining-elts (cdr right-result)])
                (cons
                 (make-tree this-entry left-tree right-tree)
                 remaining-elts))))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define (intersection-ordered-list set1 set2)
  (if (or (null? set1) (null? set2))
      `()
      (let ([x1 (car set1)] [x2 (car set2)])
        (cond
          [(= x1 x2)
           (cons (x1 (intersection-ordered-list
                      (cdr set1)
                      (cdr set2))))]
          [(< x1 x2)
           (intersection-ordered-list (cdr set1) set2)]
          [(> x1 x2)
           (intersection-ordered-list set1 (cdr set2))]))))

(define (union-ordered-list set1 set2)
  (cond
    [(null? set1) set2]
    [(null? set2) set1]
    [(= (car set1) (car set2))
     (cons (car set1)
           (union-ordered-list (cdr set1) (cdr set2)))]
    [(< (car set1) (car set2))
     (cons (car set1) (union-ordered-list (cdr set1) set2))]
    [else
     (cons (car set2)
           (union-ordered-list set1 (cdr set2)))]))

;; 这里假设 set1 set2 均为平衡二叉搜索树，他们具有特征
;; 左子树的所有元素都小于 根节点 小于右子树
;; 因此中序 traversal 的结果一定是个有序列表
(define (intersection-set set1 set2)
  (let ([list1 (tree->list set1)] [list2 (tree->list set2)])
    (list->tree (intersection-ordered-list list1 list2))))

(define (union-set set1 set2)
  (let ([list1 (tree->list set1)] [list2 (tree->list set2)])
    (list->tree (union-ordered-list list1 list2))))

#| (define tree (make-tree 1 (make-tree 2 '() '()) (make-tree 3 '() '()))) |#

(define (lookup given-key set-of-records)
  (cond
    [(null? set-of-records) #f]
    [(= given-key (entry set-of-records))
     (entry set-of-records)]
    [(> given-key (entry set-of-records))
     (lookup given-key (right-branch set-of-records))]
    [else (lookup given-key (left-branch set-of-records))]))

;; constructor
(#%provide make-tree
           ;; selectors
           entry
           left-branch
           right-branch
           ;; transformation
           tree->list
           list->tree
           ;; other helpers
           lookup
           adjoin-set
           partial-tree
           union-ordered-list
           intersection-ordered-list)
