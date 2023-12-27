#lang sicp

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

(define (element-of-set? x set)
  (cond
    [(null? set) #f]
    [(equal? x (car set)) #t]
    [else (element-of-set? x (cdr set))]))

;; 下面来实现通过 message 和 huffman tree 的方法
;; 为一个符号 生成 对应的二进制 list
;; 主要思路为，每次都检查字符在不在左子树的 symbols 里, 如果在，就往左走
;; bits + 0, 否则向右走， bits + 1
;; 如果走到 leaf 却不满足，就报错
(define (encode-symbol symbol tree)
  (if (leaf? tree)
      (if (eq? (symbol-leaf tree) symbol)
          '()
          (error "missing symbol -- ENCODE_SYMBOL" symbol))
      (let ([lb (left-branch tree)])
        (if (element-of-set? symbol (symbols lb))
            (cons 0 (encode-symbol symbol lb))
            (cons 1
                  (encode-symbol symbol
                                 (right-branch tree)))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; 和 define 的 sample message 相同
(encode (decode sample-message sample-tree) sample-tree)
(display sample-tree)
