#lang sicp

(#%require "./38.rkt")

(define (reverse sequence)
  (fold-right (lambda (acc cur) (cons acc cur)) nil sequence))

(define (reverse2 sequence)
  (fold-left (lambda (cur acc) (cons acc cur)) nil sequence))

;; 利用 fold 实现 reverse
(reverse (list 1 2 3 4))
(reverse2 (list 1 2 3 4))

