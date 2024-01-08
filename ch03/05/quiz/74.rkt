#lang sicp
(#%require "../../../common/data/stream.rkt")

;; 根据信号产生 zero-crossing stream
;; zero-crossing 也就是表示当前的信号变化经过了 0, 比如从负值变为了正值，或者从正值变为了负值
;; 例如 -1 -> 1 ，这个流的结果就是 1
;; 1 -> -1, 则为 -1
;; 1 -> 2, 则为 0, -1 -> -2 或 -3 -> -1 也均为 0
;; 可以这样描述一个过零点信号的流
;; 原始信号:  ... 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4 ...
;; 过零点信号:... 0 0  0  0  0   -1   0  0  0    0   1 0 0 ...
(define (sign-change-detector curr last)
  (cond
    [(and (> curr 0) (< last 0)) 1]
    [(and (< curr 0) (> last 0)) -1]
    [else 0]))
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream)
                         last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define sense-data
  (cons-stream
   1
   (cons-stream
    -1
    (cons-stream
     3
     (cons-stream 5 (cons-stream -2 (cons-stream -1 3)))))))

(define zero-crossings (make-zero-crossings sense-data 0))
(define zero-crossings-2
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(stream-refs 6 zero-crossings)
(stream-refs 6 zero-crossings-2)
