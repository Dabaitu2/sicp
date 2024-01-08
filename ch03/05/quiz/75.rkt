#lang sicp
(#%require "../../../common/data/stream.rkt")

(define (sign-change-detector curr last)
  (cond
    [(and (> curr 0) (< last 0)) 1]
    [(and (< curr 0) (> last 0)) -1]
    [else 0]))

;; louis version
;; the issue here is: what we need is a new stream genertaed from input stream's average
;; rather than the average of input signal and last avpt
;; and what we want to average is also the avpt stream rather than the input stream
(define (louis-make-zero-crossings input-stream last-value)
  (let ([avpt (/ (+ (stream-car input-stream) last-value)
                 2)])
    (cons-stream (sign-change-detector avpt last-value)
                 (louis-make-zero-crossings
                  (stream-cdr input-stream)
                  avpt))))

(define sense-data
  (cons-stream
   1
   (cons-stream
    -1
    (cons-stream
     3
     (cons-stream 5 (cons-stream -2 (cons-stream -1 3)))))))

(define (make-zero-crossings input-stream
                             last-value
                             last-avpt)
  (let ([avpt (/ (+ (stream-car input-stream) last-value)
                 2)])
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings
                  (stream-cdr input-stream)
                  (stream-car input-stream)
                  avpt))))

(stream-refs 6 (louis-make-zero-crossings sense-data 0))
(stream-refs 6 (make-zero-crossings sense-data 0 0))
