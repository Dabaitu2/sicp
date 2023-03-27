#lang sicp

;; ========================================================================================================================
;; 2.2.2 层次型结构
;; (cons
;;    (cons 1 (cons 2 ))
;;    (cons 3 (cons 4 ))
;; )
;; 这里打印的是 ((1,2), 3, 4)
;; 表示 cons 有一种特殊表示法叫做 dotted-pair
;; 即 (cons a b) -> (a . b)
;; 而对于 (list a b) ->  (cons a (cons b nil)) -> (a . (b . nil))
;; 而由于 lisp 具有一个化简特性
;; (a . (b ...)) -> (a b ...)
;; 因此 代换到这里是 ((1 2) . (3 4)) -> ((1 2) 3 4)
(cons (list 1 2) (list 3 4))

;; 而对于最外层是 list 的情形
;; 由于多了一层 list, cons 套 cons 变成了这样
;; ((1 2) . ((3 4) . nil)) (nil 可以直接省略)
;; ((1 2) . ((3 4))) -> ((1 2) (3 4)) 只化简一次，导致结果仍然包含括号
(list (list 1 2) (list 3 4))

;; x 是一种多个 list 构成的结构，这种叫做层次型结构
;; 和数据结构中的 tree 很相似
(define x (cons (list 1 2) (list 3 4)))
(length x) ;;  作为 list 而言, 结果为 3

;; 求叶子节点的数
(define (count-leaves x)
  (cond
    [(null? x) 0]
    [(not (pair? x))
     1] ;; pair 用于判断是不是 cons, 而树中非 pair 的就是叶子
    [else
     (+ (count-leaves (car x)) (count-leaves (cdr x)))]))

(count-leaves x) ;; 作为树而言, leaves = 4

;; 与 map 处理序列这种强有力的高阶抽象类似
;; map + 递归同样也是处理树的一种强有力的抽象
;; 下面是一种普通递归实现的 scale-tree
(define (scale-tree tree factor)
  (cond
    [(null? tree) nil]
    [(not (pair? tree)) (* tree factor)]
    [else
     (cons (scale-tree (car tree) factor)
           (scale-tree (cdr tree) factor))]))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

;; 通过结合 map, 我们可以进一步抽象它
(define (scale-tree-2 tree factor)
  (map (lambda (subtree)
         (if (pair? subtree)
             (scale-tree subtree factor)
             (* subtree factor)))
       tree))

(scale-tree-2 (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

