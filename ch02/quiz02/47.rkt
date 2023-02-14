#lang sicp

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))


(define (frame-make origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (frame-origin f) (car f))
(define (frame-edge1 f) (cadr f))
(define (frame-edge2 f) (cddr f))
