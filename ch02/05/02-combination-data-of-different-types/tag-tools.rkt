#lang racket

;; Tagged data
(define (attach-tag type-tag contents)
  (cons type-tag contents))
;; (if (number? contents) contents (cons type-tag contents)))

(define (type-tag datum)
  (cond
    [(pair? datum) (car datum)]
    [(number? datum) 'real]
    [else (error "Bad tagged datum -- TYPE-TAG" datum)]))

(define (contents datum)
  (cond
    [(pair? datum) (cdr datum)]
    [(number? datum) datum]
    [else (error "Bad tagged datum -- CONTENTS" datum)]))

(#%provide attach-tag type-tag contents)
