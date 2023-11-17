#lang racket

(#%require "../../../common/data/stream.rkt")


(define (add-stream s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))


(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-stream ones integers)))


;; 阶乘流
;; how does it work?
;; 实际上，阶乘流本身是在不停的乘上 integers 所提供的每一个 integer, 并构造成下一个元素
;; 这样就相当于缓存了之前阶乘的结果，coooool
;;
;;    1 1 2 6 24              factorial
;;    1 2 3 4 ..              integers
;; -----------------------> time goes through
;;  1 1 2 6 24 
(define factorial (cons-stream 1 (mul-streams factorial integers)))
(stream-ref factorial 0)
(stream-ref factorial 1)
(stream-ref factorial 2)
(stream-ref factorial 3)
(stream-ref factorial 4)
