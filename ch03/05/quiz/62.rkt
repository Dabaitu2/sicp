#lang sicp

;; helpers
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

(define (div-series s1 s2)
  (let ([first (stream-car s2)])
    (if (= 0 first)
        (error
         "denominator's constant item can not be zero")
        (mul-series s1 (reciprocal-series s2)))))

(define (integrate-series stream)
  (stream-map / stream integers))

(define cosine-series
  (cons-stream 1
               (stream-map -
                           (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; tan(x) = sin(x)/cos(x)
(define tan-series (div-series sine-series cosine-series))
(map (lambda (s) (stream-ref tan-series s))
     '(0 1 2 3 4 5 6 7 8 9 10))
