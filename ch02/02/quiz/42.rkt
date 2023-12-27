#lang sicp
(#%require
 "../../../common/data/conventional-interface.rkt")

;; 更多 comment 可以看 algo/eight-queen.rkt
(define empty-board '())

(define (adjoin-position new-row col rest-of-queens)
  (cons (list new-row col) rest-of-queens))

(define (safe? k positions)
  (define (check offset queen rest-of-queens)
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
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap
                 (lambda (rest-of-queens)
                   (map (lambda (new-row)
                          (adjoin-position new-row
                                           k
                                           rest-of-queens))
                        (enumerate-interval 1 board-size)))
                 (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 8))
