#lang sicp

;; Tagged data
(define (attach-tag type-tag contents)
  (cons type-tag contents))
;; (if (number? contents) contents (cons type-tag contents)))

(define (is-datum? datum)
  (cond
    [(pair? datum) #t]
    [(number? datum) #t]
    [else #f])
  )
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

(#%provide attach-tag type-tag contents is-datum?)
