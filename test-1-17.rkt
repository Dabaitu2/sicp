#lang sicp
;; 用乘法代替加法
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
(* 3 4)
;; 用类似求快速幂的方法求乘积(递归计算方法)
(define (even? n)
  (= (remainder n 2) 0))
(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))
(define (multi a b)
  (cond ((= 1 b) a)
        ((even? b) (multi (double a) (halve b)))
        (else (+ a (multi a (- b 1))))))
(multi 9 9)