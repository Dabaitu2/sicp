#lang sicp
(#%require "../../../common/data/stream.rkt")
(#%require "../../../common/math/num-theory.rkt")

;; ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               ;; 不停的利用递归：新的流的第一个元素来构造下一个元素
               ;; 同时不停的递归 stream-map 也会使得正负号恰好交替
               (stream-map - (ln2-summands (+ n 1)))))

;; 1 个元素 s1
;; 第二个 s1+s2
;; 第三个 s1+s2+s3
;; ...
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-stream (stream-cdr s)
                           (partial-sums s))))

;; 获得 ln2 的逼近值
(define ln2-streams
  (partial-sums (ln2-summands 1)))


;; 欧拉加速器
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))


(stream-refs 10 ln2-streams)
(stream-refs 10 (euler-transform ln2-streams))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))


;; 指数加速
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))


(stream-refs 10 (accelerated-sequence euler-transform ln2-streams))


