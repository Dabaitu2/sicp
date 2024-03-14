#lang sicp

;; Non-determinstic 8-queens
(define (require p)
  (if (not p) (amb)))

(define (adjoin-position new-row col rest-of-queens)
  (cons (list new-row col) rest-of-queens))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (safe? k positions)
  (define (check offset queen rest-of-queens)
    ;; 检查该点与第一个皇后位置是否相等，是否处在统一对角(横坐标不在左右offset处)（纵坐标差距肯定是offset）
    (cond
      [(= offset k) #t]
      [(= (caar rest-of-queens) (car queen)) #f]
      [(= (- (caar rest-of-queens) offset) (car queen)) #f]
      [(= (+ (caar rest-of-queens) offset) (car queen)) #f]
      [else
       (check (+ offset 1) queen (cdr rest-of-queens))]))
  (check 1 (car positions) (cdr positions)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        ;; changed => 这里仅产出一个值，所以不再返回列表
        '()
        (let ([new-row
               ;; changed => 这里只产出一个可能的值而非 枚举所有值
               (amb (an-integer-between 1 board-size))]
              ;; 这里也只会有一个可能结果, 故不使用 amb
              [rest-of-queens (queen-cols (- k 1))])
          (let ([may-solution (adjoin-position
                               new-row
                               k
                               rest-of-queens)])
            (require (safe? k may-solution))
            may-solution))))
  (queen-cols board-size))

(queens 8)
