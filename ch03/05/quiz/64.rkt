#lang sicp

(#%require "../../../common/data/stream.rkt")

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;; 获得流中符合需求的元素
(define (stream-limit stream tolerance)
  (let ([first (stream-car stream)]
        [others (stream-cdr stream)])
    (let ([second (stream-car others)])
      (if (< (abs (- first second)) tolerance)
          second
          (stream-limit others tolerance)))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 1)
(sqrt 2 0.1)
(sqrt 2 0.01)
