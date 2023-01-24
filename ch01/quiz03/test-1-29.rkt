#lang sicp

;; 辛普森积分
(define (simpson-integral f a b n)
  (define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)  ;; 对 a 做一些操作
           (sum term (next a) next b)))) ;; 此处仍然是递归
  (define (term x)
    (+ (f x) (* 4 (f (+ x h))) (f (+ x (* 2 h)))))
  (define (next x)
    (+ x (* 2 h)))
  (define h (/ (- b a) n))
  (* (/ h 3) (sum term a next (- b (* 2 h))))
  )

(define (cube x) (* x x x))

(display (simpson-integral cube 0 1 100)) (newline)
(display (simpson-integral cube 0 1 1000)) (newline)
