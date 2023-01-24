#lang sicp

(#%require "./test-1-37.rkt")

(define (e-euler k)
  (+ 2.0 (cont-frac (lambda (x) 1)
                    (lambda (x)
                      (if (= (remainder x 3) 2)
                          (/ (+ x 1) 1.5)
                          1))
                    k)))

(e-euler 10)
