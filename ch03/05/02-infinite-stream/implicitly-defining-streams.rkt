#lang racket
(#%require "../../../common/data/stream.rkt")
(#%require "../../../common/math/num-theory.rkt")
(#%require "./cases.rkt")


(define ones (cons-stream 1 ones))

(define (add-stream s1 s2)
  (stream-map + s1 s2))

;; 这里又是一个很 awesome 的实现，由于延时求值的特性
;; integers 在每一轮产出新的值的时候正好利用了上一轮的结果产出的最后一个值
;; 例如 integers 第一位 为 1.
;; 第二位为 1+1 = 2
;; 第三位则为 1+2 = 3， 这里的2就是来自于上一位的求值！
(define integers (cons-stream 1 (add-stream ones integers)))
(stream-ref integers 100)


;; 同样的，我们也能够用相同的方式延时的产生 fibonacci 数列
;;     1 1 2 3 5 8  13 21  (stream-cdr fibs)
;;     0 1 1 2 3 5  8  13  fibs
;; -------------------------------------------> time goes through
;; 0 1 1 2 3 5 8 13 21 34  fibs
(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-stream (stream-cdr fibs) fibs))))
(stream-ref fibs 8)


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;; 生成 2 的 n 次幂
(define double (cons-stream 1 (scale-stream double 2)))
(stream-ref double 4)

;; 一个数是质数，当且仅当其不能被任何小于等于 √n 的质数整除
;; 因为 2 也是质数，所以偶数天然不会满足条件
;; 这个算法相较于传统的质数检查比较快, 因为我们在这个无穷流中缓存了所有小于当前数的质数
;; 传统的质数检查需要每次都一个一个检查一次所有的数
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))
(define primes
  (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))
(stream-ref primes 0)
(stream-ref primes 1)
(stream-ref primes 2)
(stream-ref primes 3)
