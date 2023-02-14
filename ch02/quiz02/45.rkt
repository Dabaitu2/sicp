#lang sicp

(#%require "../../common/painter/wave.rkt")

;; 将 right-split 和 up-split 都可以看成一种更通用操作的抽象
;; 写出这种抽象 split
(define (split transform1 transform2)
  (define (inner painter n)
    (if (= n 0)
        painter
        (let ([smaller (up-split painter (- n 1))])
          (transform1 painter
                      (transform2 smaller smaller)))))
  (inner))

(define right-split (split beside below))
(define up-split (split below beside))
