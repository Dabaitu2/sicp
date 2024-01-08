#lang sicp

(#%require "../../../common/data/stream.rkt")

(define (sign-change-detector curr last)
  (cond
    [(and (> curr 0) (< last 0)) 1]
    [(and (< curr 0) (> last 0)) -1]
    [else 0]))

(define sense-data
  (cons-stream
   1
   (cons-stream
    -1
    (cons-stream
     3
     (cons-stream 5 (cons-stream -2 (cons-stream -1 3)))))))

;; (define (smooth stream)
;;   (let ([avpt (/ (+ (stream-car stream)
;;                     (stream-car (stream-cdr stream)))
;;                  2)])
;;     (cons-stream avpt (smooth (stream-cdr stream)))))

(define (smooth-stream stream)
  (stream-map (lambda (curr last)
                (/ (+ curr last) 2))
              stream
              (stream-cdr stream)))

(define (make-zero-crossings input-stream)
  (let ([smooth-stream (smooth-stream input-stream)])
    (stream-map sign-change-detector
                smooth-stream
                (cons-stream 0 smooth-stream))))

(define x (make-zero-crossings sense-data))
(stream-refs 5 x)
