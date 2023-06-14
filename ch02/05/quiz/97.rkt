#lang racket

;; 结合求 gcd 的操作将有理函数化简为最简形式

;; a
(define (reduce-terms n d)
  (let ((gcd-n-d (gcd-temrs n d)))
    (list (car (div-terms n gcd-n-d))
          (car (div-terms d gcd-n-d)))
    )
  )
