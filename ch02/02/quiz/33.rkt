#lang sicp

(#%require "../../common/data/conventional-interface.rkt")

;; 使用 accumulate 实现底层逻辑
(define (map proc sequence)
  (accumulate (lambda (first already-accumulated)
                (cons (proc first) already-accumulated))
              nil
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (first already-accumulated)
                (+ 1 already-accumulated)
                )
              0
              sequence))
