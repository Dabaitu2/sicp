#lang sicp

(#%require "../../../common/data/stream.rkt")

(define random-init 987654321)
(define a 1103515245)
(define c 12345)
(define m (expt 2 31))

;; 线性同余法求随机数, 这里的几个参数都是随便求的，本身应该有更好的选择
(define (rand-update x)
  (remainder (+ (* a x) c) m))

(define rand
  (let ([x random-init])
    (lambda ()
      (set! x (rand-update x))
      x)))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed))
                 (monte-carlo (stream-cdr experiment-stream)
                              passed
                              failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

;; 这里的结果不是很正确，因为我们的随机数算法不行
(stream-ref pi 100)
