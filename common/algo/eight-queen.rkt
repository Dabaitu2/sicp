#lang sicp
(#%require "../../common/data/conventional-interface.rkt")

;; k皇后问题的递归解法
;; 1. 假设k-1问题已经得到解决，形成了一些k-1序列, 针对每一个序列，对k位构成的k种可能，检查每种可能
;; 2. 如果满足这种可能，就将这种可能和k-1序列合并形成新序列
;; 3. 所以，如果要获得新序列，我们首先要获得k-1序列，于是首先去解决新序列
;; 4. 同理，要解决k-1序列，要先解决k-2... 最后终止与解决1皇后问题，1皇后问题有k个解，此时开始回溯

(define empty-board '())

;; 链接两个list
(define (adjoin-position new-row col rest-of-queens)
  (cons (list new-row col) rest-of-queens))


(define (safe? k positions)
  (define (check offset queen rest-of-queens)
    ;; 检查该点与第一个皇后位置是否相等，是否处在统一对角(横坐标不在左右offset处)（纵坐标差距肯定是offset）
    (cond [(= offset k) #t]
          [(= (caar rest-of-queens) (car queen)) #f]
          [(= (- (caar rest-of-queens) offset) (car queen)) #f]
          [(= (+ (caar rest-of-queens) offset) (car queen)) #f]
          [else (check (+ offset 1) queen (cdr rest-of-queens))]))
  (check 1 (car positions) (cdr positions))
  )


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         ;; 对可能的每一种组合形式进行过滤
         (lambda (positions) (safe? k positions))
         ;; 对余下的已经排序过的 k-1 皇后,
         ;; 将新皇后从 1 ~ board-size 位依次和 list 链接
         ;; 然后 flatmap 合并到一起去
         (flatmap (lambda (rest-of-queens)
                    (map (lambda (new-row)
                           (adjoin-position new-row
                                            k
                                            rest-of-queens))
                         (enumerate-interval 1 board-size)))
                  ;; 余下已经排序过的k-1皇后来自递归计算
                  (queen-cols (- k 1))))))
  (queen-cols board-size))

;; (queens 8)
(length (queens 8))
(list-ref (queens 8) 4)
