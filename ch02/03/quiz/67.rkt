#lang racket

;; helpers
(define (make-leaf symbol weight)
  (list `leaf symbol weight))

;; eq 是严格判断符号相等，equal 也可以用来判断数值
(define (leaf? object)
  (eq? (car object) `leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree) (cadddr tree)))

;; 归并两颗子树
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;; 利用 huffman tree 解码 bits, bits 就是 list(0, 1)
;; lisp 比较妙的一点是通常是用递归和整体数据状态的变化来推进过程的
;; 而不像一般的语言需要通过显式增加语法 比如 for 循环和额外的游标变量来控制
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        `()
        (let ([next-branch
               (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;; 是 0 就看左子树，是 1 就看右子树
(define (choose-branch bit branch)
  (cond
    [(= bit 0) (left-branch branch)]
    [(= bit 1) (right-branch branch)]
    [else (error "bad bit -- CHOOSE-BRANCH" bit)]))

(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)
