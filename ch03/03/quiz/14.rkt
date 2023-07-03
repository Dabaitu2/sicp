#lang sicp

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        ;; cdr would not hold the ref of x1
        ;; it will generate a new value from original one
        (let ([temp (cdr x)])
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))


(define v '(a b c d))
v
(define w (mystery v))
w

;; the list will be reversed
;; the progress likes below
;; temp    | x
;; --------|---------
;; (b c d) | (a ())  
;; (c d)   | (b a ())
;; (d)     | (c b a ())
;; ()      | (d c b a ())
