#lang sicp

(#%require "../../../common/data/stream.rkt")

;; helpers
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-stream (scale-stream (stream-cdr s2)
                                         (stream-car s1))
                           (mul-series (stream-cdr s1)
                                       s2))))

;; X = 1 - S_R * X
(define (reciprocal-series s)
  (cons-stream 1
               (scale-stream
                (mul-series (stream-cdr s)
                            (reciprocal-series s))
                -1)))
