#lang racket

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

;; 拿到 tree 中的所有 symool
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
;; 而不像一般的语言需要通过显式增加语法 比如 for 循环和额外的游标来控制
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

;; 上面说了那么多，但他们都基于 huffman 树已经建立好了的假设
;; 因此只提供了一些原子化的能力，为了在实际工作中可以使用 huffman tree
;; 下面是一些辅助方法，用于
;; 1. 通过比较权重的方式去合并 leaf 和 leaf-set, 生成一个有序集合, 这会用于建立 huffman tree
;; 2. 利用初始的 字符-频度 cons list 去建立起一个初始有序树叶集合, 便于开始归并

(define (adjoin-set x set)
  (cond
    [(null? set) (list x)]
    [(< (weight x) (weight (car set))) (cons x set)]
    [else (cons (car set) (adjoin-set x (cdr set)))]))

;; (list (list 'A 3) ('B 7) ('C 1)) -> (list (leaf 'C 1) (leaf 'A 3) (leaf 'B 7))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    ;; 递归生成 leaf 并且 adjoin 上
                    (make-leaf-set (cdr pairs))))))

(define (element-of-set? x set)
  (cond
    [(null? set) #f]
    [(equal? x (car set)) #t]
    [else (element-of-set? x (cdr set))]))

;; 下面来实现通过 message 和 huffman tree 的方法
;; 为一个符号 生成 对应的二进制 list
;; 主要思路为，每次都检查字符在不在左子树的 symbols 里
;; 如果在，就往左走(递归去判断 左子树)
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


;; 使用make-code-tree 反复归并集合中具有最小权重的元素,
;; 直至集合里只剩下一个元素为止。
;; 这个元素就是我们所需要的Huffman树
;; 我们在创建了 new tree 之后, 需要使用 adjoin-set 把它放回原 set 中并且确保他的顺序
(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (let ((first (car leaf-set))
            (second (cadr leaf-set))
            (rest (cddr leaf-set)))
        (successive-merge (adjoin-set (make-code-tree first second)
                                      rest)))))


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define rock-lyrics '((A 2)
                      (BOOM 1)
                      (SHA 3)
                      (GET 2)
                      (YIP 9)
                      (NA 16)
                      (JOB 2)
                      (WAH 1)))

(define code-tree (generate-huffman-tree rock-lyrics))

(define song
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))

(cons "huffman-encoding-length"
      (length (encode song code-tree)))
(cons "min fixed-length encoding length"
      (* (log (length rock-lyrics) 2) (length song)))
