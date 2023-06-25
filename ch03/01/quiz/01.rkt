#lang racket

;; 实现累加器
(define (make-accumulator initial)
  (lambda (amount)
    (set! initial (+ initial amount))
    initial))

(define A (make-accumulator 5)) (A 10) 
;; 15
(A 10)
;; 25

