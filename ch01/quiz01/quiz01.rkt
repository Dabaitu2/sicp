#lang racket

;; quiz 1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))


;; quiz 1.3
;; 组合过程
(define (bigger x y)
  (if (> x y)
      x
      y))

(define (smaller x y)
  (if (< x y)
      x
      y))

(define (max_sum x y z)
  (+ (bigger x y)
     (bigger (smaller x y) z)))


;; 使用 predicate·
(define (max_sum_predicate x y z)
  (if (> x y)
      (+ x
         (if (> y z)
             y
             z))
      (+ y
         (if (> x z)
             x
             z))))

;; quiz 1.4
;; 运算符都可以作为表达式被返回
;; 这个函数支持根据不同的情况使用不同的表达式进行组合
(define (a-plus-abs-b a b)
  ((if (> b 0)
       +
       -) a b))


;; quiz 1.7
;; 不再检测猜测值 guess 的平方与 x 之间的差,
;; 而是检测新旧两次猜测值之间的比率, 当比率变化非常小时,
;; 程序就停止 improve 。
(define (good-enough? old-guess new-guess)
  (> 0.01
     (/ (abs (- new-guess old-guess))
        old-guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))  ; 调用新的 good-enough?
      (improve guess x)
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess x (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))


;; quiz 1.8
;; 牛顿法求立方根
(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve-cube guess x)
                      x)))

(define (improve-cube guess x)
  (/ (+ (/ x
           (* guess
              guess))
        (* 2
           guess))
     3))
