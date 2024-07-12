#lang sicp

;; Why does flatten-stream use delay explicitly?
;; I think it's the same question with 4.71 & 4.72
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave (stream-car stream)
                  (flatten-stream (stream-cdr stream)))))
