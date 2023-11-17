#lang sicp
(#%require "../../../common/data/stream.rkt")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-stream ones integers)))

(define (add-stream s1 s2)
  (stream-map + s1 s2))

;;  0  1  2     3   4           s
;;     0  01    012 0123        output
;; ----------------------->   time goes through
;;  0  01 0+1+2 0123 01234      output

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-stream (stream-cdr s)
                           (partial-sums s))))

(define s (partial-sums integers))

(stream-ref s 0)
(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)
