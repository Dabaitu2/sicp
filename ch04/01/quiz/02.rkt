#lang sicp

;; a)
;; louis want to change the implementation of eval so that application clause can be put ahead
;; but it couldn't work
;; cause it simply judge if the exp is an application by checking if it is a pair
;; and almost every exp is a pair, so they can't be recognized as corresponding syntax part correctly


;; b)
(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp)
  (cadr exp))
(define (operands exp)
  (cddr exp))
(define (no-operands? ops)
  (null? ops))
(define (first-operand ops)
  (car ops))
(define (rest-operands ops)
  (cdr ops))




