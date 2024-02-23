#lang sicp

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (not (eq? x true)))

(#%provide true? false?)
