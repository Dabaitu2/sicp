#lang sicp

(define (factorial n)
  (if (= n 1) 1 (* (factorial (- n 1)) n)))

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))
