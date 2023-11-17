#lang sicp

;; 流的目标就是，我们看起来还是在操作 list (cons a (cons b (cons c nil)))
;; 但他每次都只会针对这其中我们需要的那部分的数据进行操作/求值。

;; wishful thinking!

;; 构造一个流
;; (define (cons-stream x y)
;;   (display "TODO"))

;; (stream-car (cons-stream x y)) = x
(define (stream-car stream)
  (display "TODO"))

;; (stream-cdr (cons-stream x y)) = y
(define (stream-cdr stream)
  (display "TODO"))

;; 类似于 list-ref 用于求流中的某一项
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (list-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc stream-car s)
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))
