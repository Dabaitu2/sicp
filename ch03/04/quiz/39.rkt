#lang sicp
(#%require "../01-nature-of-time-in-concurrent-systems/serializer.rkt")

(define x 10)
(define s (make-serializer))
(parallel-execute (lambda ()
                    (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

;; p1 -> p1 set,  p1 get (it could not be seperated)
;; p2

;;  p1-get -> p2 -> p1 set --> 100
;;  p2 -> p1 --> 121
;;  p1 -> p2 --> 101
