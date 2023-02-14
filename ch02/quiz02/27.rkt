#lang sicp

(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (let ([first (car items)])
                (cons (if (pair? first)
                          (iter first nil)
                          (car items))
                      result)))))
  (iter items nil))

(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
        result
        (reverse-iter (cdr items)
                      (cons (car items) result))))
  (reverse-iter items nil))

(reverse x) ;; -> ((3 4) (1 2))
(deep-reverse x) ;; ->  ((4 3) (2 1))
