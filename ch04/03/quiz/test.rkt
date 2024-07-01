#lang sicp

(define target 1)

(define (try)
  (let ([var (amb 7 8 9)])
    (display "old-var: ")
    (display target)
    (newline)
    (set! target var)
    (display "new-var: ")
    (display target)
    (newline)
    (display "===============")
    (newline)
    target))

(try)
(amb)
(amb)
