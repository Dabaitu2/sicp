#lang sicp

(define a 1)
(define b 2)

(list a b)
(list 'a 'b)
(list 'a b)
(list a 'b)

(car '(a b c))
(cdr '(a b c))

(define nil '())

(define (memq item x)
  (cond
    [(null? x) false]
    [(eq? item (car x)) x]
    [else (memq item (cdr x))]))

(memq 'apple `(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))
