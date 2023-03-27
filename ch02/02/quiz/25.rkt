#lang sicp

(define x1 (list 1 3 (list 5 7) 9))
(define getx1 (car (cdr (car (cdr (cdr x1))))))
(display getx1)
(newline)

(define x2 (list (list 7)))
(define getx2 (car (car x2)))
(display getx2)
(newline)

(define a (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr a))))))))))))
