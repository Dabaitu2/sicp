#lang sicp

(#%require "../../../common/data/stream.rkt")
(#%require "../../../common/math/num-theory.rkt")

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed))
                 (monte-carlo (stream-cdr experiment-stream)
                              passed
                              failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-in-range low high)
  (let ([range (- high low)]) (+ low (random range))))

(define (estimate-integral-stream P x1 x2 y1 y2)
  (define (is-in-integral-function? _)
    (let ([x (random-in-range x1 x2)]
          [y (random-in-range y1 y2)])
      (P x y)))
  (let ([base-area (* (- x2 x1) (- y2 y1))])
    (stream-map (lambda (x) (* base-area x))
                (monte-carlo
                 (stream-map is-in-integral-function? ones)
                 0
                 0))))

;; test
(define (circle-test x y)
  (>= 9 (+ (square (- x 5)) (square (- y 7)))))

(stream-ref
 (estimate-integral-stream circle-test 2 8 4 10)
 10000) ;; 27-28, while the real aera is close to 28.27
