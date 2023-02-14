#lang racket
;; test-1-11
;; 递归过程计算
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2
                    (f (- n 2)))
                 (* 3
                    (f (- n 3)))))))
;; 迭代过程计算
;; a代表计算的g(n+2)=f(n-1) b=g(n+1)=f(n-2) c=g(n)=f(n-3)
(define (g n)
  (g-iter 2 1 0 0 n))
;; i 迭代指针 n输入n，迭代终止条件
(define (g-iter a b c i n)
  (if (= i n)
      c
      (g-iter (+ a (* 2 b) (* 3 c))
              a
              b
              (+ i 1)
              n)))

(g 3)
