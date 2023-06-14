#lang racket

;; only reduce fraction on condition that n & d are all integer
(define (make-rat n d)
  (if (and (integer? n) (integer? d))
      (let ([g (gcd n d)]) (cons (/ n g) (/ d g)))
      (cons n d)))
