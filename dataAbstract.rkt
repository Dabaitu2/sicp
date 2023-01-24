#lang racket
(#%require "common.rkt")
(define one-through-four (list 1 2 3 4))
one-through-four
(car one-through-four)
(cdr one-through-four)
(cons (cons 6 7) one-through-four)

;; 实现类似于求数组某一项的操作
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 3)
;; 实现求数组长度的操作
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(length squares)
;; 使用迭代方案
(define (length2 items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(length2 squares)
;; 数组的append 操作
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(append squares squares)
;; 高阶过程map
(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))
(map (lambda (x) (* x x)) (list 1 2 3 4))
;; 求cons 实际对应的树的叶子结点数目
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(count-leaves (cons (list 1 2 3) (list 5 6)))
;; 对树的map操作
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
;; filter 操作
;; 如果predicate 通过就增加到cons中否则抛弃，继续检查子cons
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
;; 实现类似reduce的操作
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
(accumulate cons null (list 1 2 3 4 5))
;; 枚举一棵树
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
;; 一颗用链表模拟的树
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
;; 具有流式处理过程的计算
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
;; 求集合map操作后各个元素组成的新list
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))
;; 求出1到n之间所有小于等于n的数组成的二元组
(define (get-cons n)
  (accumulate append
              null
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
;; 判断和是否为质数
;; cadr (1 2 3) = car (cdr 1 2 3) = 2
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;; 将序对和其结果联系起来
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; 获得和为质数的1《i《j《n 序对
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                ;; 获得比i小的所有数j，并和i组成cons
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
(prime-sum-pairs 5)

;; 获得序列的全排列
(define (permutations s)
  (if (null? s)                    ; empty set?
      (list null)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
(permutations (list 2 3 4))
(reverse '(1 2 3))
