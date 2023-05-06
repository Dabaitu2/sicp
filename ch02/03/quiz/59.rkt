#lang racket

;; 使用 unordere list 表示
;; helpers
;; O(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;; O(n)
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; O(n^2) if (= (length set1) (length set2))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) `())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))


(element-of-set? 2 (list 1 2 3 4))
(adjoin-set 'a (list 1 2 3))
(union-set (list 1 2 3 4) (list 1 2 3 5))
(intersection-set (list 1 2 3 4) (list 1 2 3 5))

