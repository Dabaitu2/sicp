#lang sicp

(define (last-pair items)
  (let ([rest (cdr items)])
    (if (null? rest)
        items
        (last-pair rest))))

(last-pair (list 1 2 3 4))
