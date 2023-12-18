#lang sicp

(#%require "../../../common/data/stream.rkt")

;; helpers
(define (integrate-series stream)
  (stream-map / stream integers))
(define cosine-series
  (cons-stream 1
               (stream-map -
                           (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; 实现级数的求积
;; 递归：还是递归
;; 无穷级数乘积本质上就是将其中的一个无穷级数(s2)乘另一个无穷级数的每一项(stream-car s1)...
;; (scale (strean-car s1) (s2)
;; 再将结果加起来, 这是一个递归过程，我们的目标是一个无穷流
;; 之所以要写成这种表现，是为了使得我们无穷流中最前面的元素始终是规则中最先应该计算的元素
;; 保证每次调用无穷流的结果都是正确的
(define (mul-series s1 s2)
  ;; 第一个元素相乘，这是流中这次真实计算出的结果
  (cons-stream (* (stream-car s1) (stream-car s2))
               ;; 然后计算 s1 第一个元素和 s2 后续所有元素的乘积，这是一个流
               (add-stream (scale-stream (stream-cdr s2)
                                         (stream-car s1))
                           ;; 最后，和 s1 后续部分和 s2 所有元素的乘积流相加
                           (mul-series (stream-cdr s1)
                                       s2))))

;; 通过 sinx^2 + cosx^2 = 1 来校验
;; 1 的无穷级数为 1,0,0,0 ....
(define circle-series
  (add-stream (mul-series cosine-series cosine-series)
              (mul-series sine-series sine-series)))

(map (lambda (s) (stream-ref circle-series s))
     '(0 1 2 3 4 5 6 7 8 9 10))
