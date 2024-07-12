#lang sicp
(#%require "../../../common/data/stream.rkt")

;; 一个更简单的 stream-flatmap, 用于处理 stream 中的元素只是 singelton stream 或者 empty stream 的情况
;; 不需要去交错 interleave 了, 也不用去处理 delayed 
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

;; 只需要找到非空的 stream, 然后取出其中的第一个也是唯一一个元素, 再用 stream-map 进行拼接就好了
(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (x)
                               (not stream-null? x))
                             stream)))

;; 将此逻辑用于替换原始逻辑不会改变代码执行的表现
