#lang sicp

;; helpers
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

(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
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

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ([w (* (/ p 100.0) c)])
    (make-center-width c w)))

(define (percent c)
  (* (/ (width c) (center c)) 100))


;; 这两种方法应用到针对区间的算数时是不等价的
;; 简单来说这是因为 par1 的做法使得 r1r2 产生了相关性(直接相加，相乘)
;; 基于了对于 r1r2 相关的错误假设，相乘是一个没有实际对应意义的操作
;; 它算出了一个没有意义的上下界
;;
;; 而 par2 中 r2r2 在完成最终运算前没有产生相互的交集, 因此它的结果是正确的
;; https://www.inchmeal.io/sicp/ch-2/ex-2.14.html
;; (define (par1 r1 r2)
;;   (div-interval (mul-interval r1 r2)
;;                 (add-interval r1 r2)))
;;
;; (define (par2 r1 r2)
;;   (let ((one (make-interval 1 1)))
;;     (div-interval
;;      one (add-interval (div-interval one r1)
;;                        (div-interval one r2)))))
;;
(define I1 (make-center-percent 25 1.5))
(define I2 (make-center-percent 75 0.75))
;; (define rs1 (par1 I1 I2))
;; (define rs2 (par2 I1 I2))
;;
;; 由于 div 的内部没有让 interval 保证独立性，这里的 div 无法获得正确结果
(display-interval (div-interval I1 I1))
(display-interval (div-interval I1 I2))
;; (display-interval rs1)
;; (display-interval rs2)
