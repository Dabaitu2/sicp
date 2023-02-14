#lang sicp

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

;; x / y => x * 1 / y
(define (div-interval x y)
  (if (<= 0 (* (lower-bound y) (upper-bound y)))
      (error "Division error (interval spans 0)" y)
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (make-interval a b)
  (cons a b))

(define (lower-bound c)
  (min (car c) (cdr c)))

(define (upper-bound c)
  (max (car c) (cdr c)))

(define (display-interval i)
  (newline)
  (display "[")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))

(define i (make-interval 2 7))
(upper-bound i)
(lower-bound i)

(define j (make-interval 8 3))
(upper-bound j)
(lower-bound j)

;; Usage
(display-interval i)
(display-interval (sub-interval i j))
(display-interval (sub-interval j i))



