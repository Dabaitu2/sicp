#lang sicp

;; 不使用 min max primitive operator 完成区间乘法
(define (make-interval a b)
  (cons a b))

(define upper-bound cdr)
(define lower-bound car)
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (add-interval x
                (make-interval (- 0 (upper-bound y))
                               (- 0 (lower-bound y)))))
(define (subtract-interval x y)
  (define p1 (- (lower-bound x) (lower-bound y)))
  (define p2 (- (lower-bound x) (upper-bound y)))
  (define p3 (- (upper-bound x) (lower-bound y)))
  (define p4 (- (upper-bound x) (upper-bound y)))

  (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))

;; 由于区间可能存在负数，因此我们需要考虑所有情况
;; 假设两个区间分别为 [x1, x2] [y1, y2]
;; 如果 x1 >= 0 , 最终 乘积区间 就可能落到这几个情况
;;      1. y1 >= 0 (x1y1, x2y2)
;;      2. y1 <= 0, y2 > 0 (x2y1, x2y2)
;;      3. y2 <= 0 (x2y1, x1y1)
;; 如果 x1 < 0, x2 >= 0
;;      1. y1 >= 0, (x1y2, x2y2)
;;      2. y1 <= 0, y2 > 0 (x2y1, x1y1)
;;      3. y2 <= 0,  (x2y1, x1y1)
;; 如果 x2 < 0
;;      1. y1 >= 0, (x1y2, x2y1)
;;      2. y1 <= 0, y2 >= 0 (x1y2, x1y1)
;;      3. y2 <= 0 (x2y2, x1y1)

(define (mul-interval x y)
  (let ([x1 (lower-bound x)]
        [x2 (upper-bound x)]
        [y1 (lower-bound y)]
        [y2 (upper-bound y)])
    (cond
      [(>= x1 0)
       (cond
         [(>= y1 0) (make-interval (* x1 y1) (* x2 y2))]
         [(<= y2 0) (make-interval (* x2 y1) (* x1 y1))]
         [else (make-interval (* x2 y1) (* x2 y2))])]
      [(<= x2 0)
       (cond
         [(>= y1 0) (make-interval (* x1 y2) (* x2 y1))]
         [(<= y2 0) (make-interval (* x2 y2) (* x1 y1))]
         [else (make-interval (* x1 y2) (* x1 y1))])]
      [else
       (cond
         [(>= y1 0) (make-interval (* x1 y2) (* x2 y2))]
         [(<= y2 0) (make-interval (* x2 y1) (* x1 y1))]
         [else
          (make-interval (min (* x1 y2) (* x2 y1))
                         (max (* x1 y1) (* x2 y2)))])])))

(define int1 (make-interval 2 4))
(define int2 (make-interval 3 6))
(mul-interval int1 int2)
