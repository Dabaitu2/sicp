#lang racket

(#%require
 "../../../common/data/conventional-interface.rkt")
(#%require
 "../01-stream-as-delayed-lists/delay-and-force.rkt")

;; 实现推广形式的 stream-map, 支持接受多个参数
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       ;; 处理每一个 stream 的第一个数据，并求值
       (apply proc (map stream-car argstreams))
       ;; cons-stream 会使得这里的数据先不被求值，因为包裹在 delay 里面了
       ;; 如果需要调用时，这里会被求值，此时
       ;; 产出每一个 stream 的 cdr 数据,
       ;; 求值获得下一个 stream, 然后交给 stream map 做进一轮调用
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define s1 (stream-enumerate-interval 1 5))
(define s2 (stream-map (lambda (x) (* x 2)) s1))

(display (stream-car s2))
(newline)
(display (stream-car (stream-cdr s2)))
