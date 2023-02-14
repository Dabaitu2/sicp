#lang sicp

(#%require "../02.rkt")

;; 更为一般化的 accumulate, 结合 filter 能力
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter))
          (combiner null-value (filtered-accumulate combiner null-value term (next a) next b)))))

(define (sum-of-prime a b)
  (filtered-accumulate + 0 identity a inc b prime?))

(sum-of-prime 1 10)

