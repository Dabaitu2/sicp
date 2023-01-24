#lang racket
(#%require "common.rkt")
;; k皇后问题的递归解法
;; 1. 假设k-1问题已经得到解决，形成了一些k-1序列, 针对每一个序列，对k位构成的k种可能，检查每种可能
;; 2. 如果满足这种可能，就将这种可能和k-1序列合并形成新序列
;; 3. 所以，如果要获得新序列，我们首先要获得k-1序列，于是首先去解决新序列
;; 4. 同理，要解决k-1序列，要先解决k-2... 最后终止与解决1皇后问题，1皇后问题有k个解，此时开始回溯
(define empty-board '())
;; 如果当前的位置
(define (ok? v pos offset)
  (cond
    ;; pos为空意味着是1皇后，必然为true
    ((null? pos) #t)
    ;; 检查该点与第一个皇后位置是否相等，是否处在统一对角(横坐标不在左右offset处)（纵坐标差距肯定是offset）
    ((and (not (= v (car pos)))
          (not (= v (- (car pos) offset)))
          (not (= v (+ (car pos) offset))))
     ;; 若不是，即通过检查，递归看下一个点是否符合
     (ok? v (cdr pos) (+ 1 offset)))
    (else #f)))
;; 链接两个list
(define (adjoin-position new-row rest-of-queens)
  (append rest-of-queens (list new-row)))
;; 颠倒一下k-1皇后排序，方便ok函数执行
(define (safe? k positions)
  (let ((rev_positions (reverse positions)))
    (ok? (car rev_positions) (cdr rev_positions) 1)))
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         ;; 对可能的每一种组合形式进行过滤
         (lambda (positions) (safe? k positions))
         (flatmap
          ;; 对余下的已经排序过的k-1皇后, 将新皇后从1-board-size位依次和list链接，然后flatmap合并到一起去
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row rest-of-queens))
                 (enumerate-interval 1 board-size)))
          ;; 余下已经排序过的k-1皇后来自递归计算
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(length (queens 11))
