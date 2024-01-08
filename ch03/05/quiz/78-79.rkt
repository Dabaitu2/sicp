#lang sicp

(#%require "../../../common/data/stream.rkt")

;; 求解一类 齐次二阶线性微分方程 
(define (solve-2nd a b dt y0 dy0)
  (display "")
  (define y (delay-integral (delay dy) y0 dt))
  (define dy (delay-integral (delay ddy) dy0 dt))
  (define ddy
    (add-stream (scale-stream dy a) (scale-stream y b)))
  y)

(stream-ref (solve-2nd 1 0 0.0001 1 1) 10000)

;; 求解通用 齐次二阶线性微分方程 
;; I hate that I've not studied math well
;; so that I don't know if it works correctly
(define (general-solve-2nd dt y0 dy0 f)
  (display "")
  (define y (delay-integral (delay dy) y0 dt))
  (define dy (delay-integral (delay ddy) dy0 dt))
  (define ddy
    (stream-map f dy y))
  y)

