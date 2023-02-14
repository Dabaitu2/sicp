#lang sicp

;; 此题目目前都还没有最为标准的答案
;; 一种极端解法为仅仅评估 queen-cols 的执行次数
;; 对于 2.42 解法，queen-cols 最底层会执行 1 次
;; 对于 2.42 解法，queen-cols 最底层会执行 board^board 次
;; 所以上弦时间差距为 board-size^board_size 倍
;;
;; 而如果考虑 queen-cols 之外的部分, 两者的差距最小为 board-size 倍
;;
;; 2.43 的算法
;; 令产生最小操作 adjoin 需要时间 t, 棋盘大小为 S
;; Q1 = S * t
;; Q2 = S * Q1 + Q1
;; Q3 = S * Q2 + Q2
;; ...
;; Q8 = S^7 Q1 + Q7 = (S^S + ... S)Q1 + .... Q1
;; (flatmap
;;  (lambda (new-row)
;;    (map (lambda (rest-of-queens)
;;           (adjoin-position new-row k rest-of-queens))
;;         (queen-cols (- k 1))))
;;  (enumerate-interval 1 board-size))
;;
;; 2.42 的算法
;; 令产生最小操作 adjoin 需要时间 t, 棋盘大小为 S
;; Q1 = S * t
;; Q2 = S^2 * t + Q1
;; Q3 = S^2 * t + Q2
;; ...
;; Q8 = S^7 * t + Q7 
;; (flatmap
;;  (lambda (rest-of-queens)
;;    (map (lambda (new-row)
;;           (adjoin-position new-row k rest-of-queens))
;;         (enumerate-interval 1 board-size)))
;;  (queen-cols (- k 1)))
;; 
