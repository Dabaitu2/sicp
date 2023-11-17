#lang sicp

(#%require "../../../common/data/stream.rkt")

(define (merge s1 s2)
  (cond
    [(stream-null? s1) s2]
    [(stream-null? s2) s1]
    [else
     (let ([s1car (stream-car s1)] [s2car (stream-car s2)])
       (cond
         [(< s1car s2car)
          (cons-stream s1car (merge (stream-cdr s1) s2))]
         [(> s1car s2car)
          (cons-stream s2car (merge s1 (stream-cdr s2)))]
         [else
          (cons-stream s1car
                       (merge (stream-cdr s1)
                              (stream-cdr s2)))]))]))

;; 这样形成的 s 中的元素，不会包含 2，3，5 以外的素数因子
(define s
  (cons-stream 1
               (merge (merge (scale-stream s 2)
                             (scale-stream s 3))
                      (scale-stream s 5))))

(stream-ref s 0)
(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)
(stream-ref s 4)
(stream-ref s 5)
(stream-ref s 7)
