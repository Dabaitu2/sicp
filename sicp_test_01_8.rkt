#lang sicp
;; 计算立方
(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (good-enough? old-guess new-guess)
  (< (/ (abs (- old-guess new-guess)) old-guess) 0.0001))
;; 求立方根的近似值
(define (cube-improve old-guess x)
  (/ (+ (* 2 old-guess) (/ x (square old-guess))) 3))
(define (cube-iter guess x)
  (if (good-enough? guess (cube-improve guess x))
      guess
      (cube-iter (cube-improve guess x) x)))
(define (cube-root x)
  (cube-iter 1.0 x))
(cube-root 213212312)