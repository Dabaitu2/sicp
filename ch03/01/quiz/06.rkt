#lang racket

;; new rand procedure which support reset /  generate

(define random-init 987654321)
(define a 1103515245)
(define c 12345)
(define m (expt 2 31))

(define (rand-update x)
  (remainder (+ (* a x) c) m))

(define rand
  (let ([x random-init])
    (lambda (m)
      (cond
        [(eq? m 'generate)
         (set! x (rand-update x))
         x]
        [(eq? m 'reset)
         (lambda (new-init) (set! x new-init))]
        [else (error "Unknown request -- RAND " m)]))))

(rand 'generate) 
(rand 'generate) 
((rand 'reset) 0) 
;; ((rand 'reset) 898989) 
(rand 'generate) 
