
#lang sicp

;; ========================================================================================================================
;; 2.2.1 序列
;; list 是 cons (1 cons (2 cons (..))) 这种嵌套写法的便捷表示
;; lisp 中 cons 这种 数据组合方式 具有闭包性质
;; 就是表明他组合其数据对象得到的结果本身还可以通过同样的操作进行组合
;; 这也是抽象代数中的定义
;; lisp 中闭包还有另一层定义，表示了一种为表示自由的变量的过程使用的计数
;; 这也就是 JS 中所说的闭包
;; (JS 的函数捕获了外部的变量使得其不能被垃圾回收, 而这个变量并不是 JS 函数定义的约束变量)
;; 我们可以把序列近似看成链表或数组
(define one-through-four (list 1 2 3 4))
;; 以上的结果会得到一个 list (1, 2, 3, 4), 这个表并不等于 (list 1 2 3 4)
;; 这个过程，如果直接调用 （1，2，3，4） 1 会被认为是过程中的一部分导致出错
;; 我们把这个表理解成一个美化输出就行了

;; 和 cons 一样，我们可以用 car / cdr 去取出第一项的数据和后面的 cons
;; 从这里也可以看出 list 本身就是 cons 的反复组合
(car one-through-four) ;; 1
(cdr one-through-four) ;; (2,3,4)
(cons 10 one-through-four) ;;(10, 1, 2, 3, 4)

;; 实现类似于求数组某一项的操作
;; 还是递归, 但是是尾递归，所以性能应该没问题
(define (list-ref items n)
  (if (= n 0) (car items) (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 3) ;; 16

;; 实现求数组长度的操作
;; null 是 scheme 提供的，用于检查参数是不是空 list
(define (length items)
  (if (null? items) 0 (+ 1 (length (cdr items)))))
(length squares)

;; 使用迭代方案（本质上就是尾递归
(define (length2 items)
  (define (length-iter a count)
    (if (null? a) count (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(length2 squares)

;; 数组的append 操作
;; 组合两个 list
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(append squares squares)

;; list 的高阶过程
;; 正常情况下需要缩放一个 list 需要这么做
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)

;; 这样的操作可以用 map 来抽象
;; 针对 list 中的每一个元素依次应用 proc 过程
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))
(define (scale-list-2 items factor)
  (map (lambda (x) (* x factor)) items))

(scale-list-2 (list 2 3 4 5 6) 10)
