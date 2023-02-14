#lang sicp

(#%require "./test-1-37.rkt")

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1) x (- (* x x))))
             (lambda (i)
               (- (* i 2) 1))
             k))

(tan-cf 10)
