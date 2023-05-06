#lang racket

;; 传统的定长编码 (fixed-length) 对所有字符采用相同长度的编码位数，
;; 而对于某些场景下，不同的字符在信息中出现的密度和频次不同，他们可以利用不同长度的编码来表示，从而节省空间，这就是变长编码 (variable-length code)
;; 例如 摩尔斯电码 中针对最常见的字母 E 就只用一个点表示，不同的字符的点和划是不同的

;; 变长编码在使用上存在一个问题：我们难以判断如何到了一个字符的结束
;; 因此有两种解决方案：1. 使用固定的特殊分隔符 2. 所有字符的完整编码都不会是其他编码的前缀。
;; 例如对于如下编码, 任何一个字符都不会是其他字符的前缀。因此我们根据匹配关系一旦找到编码就可以立刻结束当前识别
;; A 0    B 100
;; C 1010 D 1011

;; 但是，我们一般无法在通信之前就知道编码的信息频度，为了实现频度统计和变长编码的结合。
;; 可以引入 Huffman 编码来做这个事情
;; Huffman 编码本质上是一颗二叉树，
;; 树叶是被编码的符号
;; 非叶节点代表了一个集合，包含了这一节点只下所有树叶上的符号
;; 位于树叶上的每一个符号还被赋予了一个权重，就是其频度，非叶节点上的权重是其下各个叶子结点的权重和
;;
;; 例如，对于一段信息 BACADAEAFABBAAGAH
;; 通过统计我们可以知道各个字符的出现频次
;; A: 8   B: 3  C: 1  D: 1
;; E: 1   F: 1  G: 1  H: 1

;; 通过 Huffman 编码，最终会产生一颗这样的树
;; [ABCDEFGH] 17
;; /           \
;; A 8         BCDEFGH 9
;;             /        \
;;            BCD 5       EFGH 4
;;           /  \           /    \
;;          B 3  CD 2     EF 2     GH 2
;;               / \       / \       / \
;;              C 1 D 1   E 1  F 1  G 1  H 1

;; 这样一颗 Huffman Tree 的意义是用来寻找编码, 有点像是字典树 + 二叉查找树 的感觉
;; 我们设定规则: 选择左子树，则编码加0，选择右子树，则编码加1
;; 在这样的情况下，各个元素的编码则为
;; A: 0      B: 100    C: 1010   D: 1011
;; E: 1100   F: 1101   G: 1110   H: 1111

;; 同样的，当我们遇到一些编码如 10001010, 我们也可以根据权重大小不停地左右左右直到找到叶子节点
;; 最终得到字符解码为 BAC

;; 那么最后的问题是，如何构造这样一颗 Huffman tree?
;; 生成 huffman tree 的算法实际上十分简单，
;; 设法安排这颗树，使得那些带有最低频度的符号出现在离树根最远的地方。
;; 这一构造过程从叶结点的集合开始，这种结点中包舍 各个符号和它们的频度，这就是开始构造编码的初始数据。
;; 现在要找出两个具有最低权重叶子，并归并它们，产生出一个以这两个结点为左右分支的结点。
;; 新结点的权重就是那两个结点的权重之和。现在我们从原來集合里删除前面的两个叶结点，并用这一新结点代替它们。

;; 例如通过下面的过程就可以生成上面的 Huffman Tree
;;
;; Initialleaves {(A8)(B3)(C1)(D1)(E1)(F1)(G1)(H1)}
;; Merge {(A8)(B3)({CD}2)(E1)(F1)(G1)(H1)}
;; Merge {(A8)(B3)({CD}2)({EF}2)(G1)(H1)}
;; Merge {(A8)(B3)({CD}2)({EF}2)({GH}2)}
;; Merge {(A8)(B3)({CD}2)({EFGH}4)}
;; Merge {(A8)({BCD}5)({EFGH}4)}
;; Merge {(A8)({BCDEFGH}9)}
;; Finalmerge {({ABCDEFGH}17)}
;;
;; 由于在寻找最小节点的过程中找到的最小节点可能不是同一个，因此生成的树是不唯一的
;; 同时，两个节点的顺序也是任意的，可以随便放到左分支或者右分支

;; 下面开始实现 constructor 和 selector
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

;; same as sample tree in 68.rkt
;; (generate-huffman-tree '((A 4) (B 2) (D 1) (C 1)))
