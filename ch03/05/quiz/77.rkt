#lang sicp

(#%require "../../../common/data/stream.rkt")

;; a modifed version which is more like the integers-starting-from
;; actually just a implicit definition of infinite stream
;; (define (integers-starting-from n)
;;   (cons-stream n (integers-starting-from (+ n 1))))
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt
                                     (stream-car integrand))
                                  initial-value)
                               dt))
                 )
               ))
(define (solve f y0 dt)
  ;; 不这样做会报错，可能跟 racket 实现有关系
  (display "")
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)


(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
