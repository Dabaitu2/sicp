#lang sicp

(#%require "../../../common/data/stream.rkt")

(define (expand num den radix)
  (cons-stream
   ;; 获得商
   (quotient (* num radix) den)
   ;; 连接生成余数, 除数，基数
   (expand (remainder (* num radix) den) den radix)))

;; I don't get what does this quiz ask for acutually ;)
(define expand-stream (expand 1 7 10))
(stream-ref expand-stream 0)
(stream-ref expand-stream 1)
(stream-ref expand-stream 2)

