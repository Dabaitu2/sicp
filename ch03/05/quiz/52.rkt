#lang racket

(#%require
 "../01-stream-as-delayed-lists/delay-and-force.rkt")

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq
  (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

;; (display-stream seq)
;; (newline)
;; (display "sum:")
;; (display sum)
;; (newline)
(stream-ref y 7)
;; (newline)
;; (display "sum:")
;; (display sum)
;; (newline)
(display-stream z)
;; (newline)
;; (display "sum:")
;; (display sum)

;; 如果我们不把 delay 做 memo 的话
;; 响应结果会不会有什么不同?
;; 由于 accum 的代码被一直内嵌在 stream-map 中的 cons-stream 中
;; 一旦针对 seq 求值，这些 accum 会再次被调用
;; 导致值越来越大
;; 这里我们也会看到，每次调用任何的求值动作都会使得整个流重播
;; 这也很符合 rx 相关逻辑的表现
