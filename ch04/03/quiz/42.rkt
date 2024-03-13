#lang sicp

(define (require p)
  (if (not p) (amb)))

;; 判断是否均是独立 item
(define (distinct? items)
  (cond
    [(null? items) true]
    [(null? (cdr items)) true]
    [(member (car items) (cdr items)) false]
    [else (distinct? (cdr items))]))

;; 使用异或运算符使得传入的参数必为一真一假
(define (xor a b)
  (if a
      (not b)
      b))

(define (multiple-dwelling)
  (let ([betty (amb 1 2 3 4 5)]
        [ethel (amb 1 2 3 4 5)]
        [joan (amb 1 2 3 4 5)]
        [kitty (amb 1 2 3 4 5)]
        [mary (amb 1 2 3 4 5)])
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))


(define stime (runtime))
(multiple-dwelling)
(display (list "Time Taken: " (- (runtime) stime)))
