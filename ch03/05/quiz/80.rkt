#lang sicp

(#%require "../../../common/data/stream.rkt")

(define (RLC R L C dt)
  (lambda (il0 vc0)
    (display "")
    (define vc (delay-integral (delay dvc) vc0 dt))
    (define il (delay-integral (delay dil) il0 dt))
    (define dil
      (add-stream (scale-stream vc (/ 1 L))
                  (scale-stream il (/ (- R) L))))
    (define dvc (scale-stream il (/ (- 1) C)))
    (stream-map cons vc il)))

(define RLC1 ((RLC 1 0.2 1 0.1) 0 10))
(stream-refs 10 RLC1)
