#lang sicp

(#%require "../../../common/data/stream.rkt")

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))


;; Louis Reasoner'version to get approximate value of sqrt 2 by using infinite stream
;; why does it not that efficient than sqrt-stream-better?
;; because it will calculate sqrt-improve twice for each element in the stream 
;; (sqrt-stream-better will only calculate once)
;; 简单来说：为了获得 sqrt-stream 中的每一个元素，在使用 stream-cdr 时，
;; 表达式 (stream-map ...) 会被求值，而这其中的 (sqrt-stream x) 同样会被求值，
;; 而这次求值，需要重新走一遍 sqrt-stream 的流程(且由于是新建函数而不是直接调用 guess，无法利用缓存)，所以效率低下。
;; 另外，如果我们的 delay 没有缓存，那么在计算 better 版本的数据时，也会出现冗余计算, 那么两者就没有什么效率上的区别了
(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))
(define (sqrt-stream-better x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(sqrt-stream-better 2)
