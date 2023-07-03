#lang sicp

(define (append x y)
  (if (null? x) y (cons (car x) (append (cdr x) y))))

;; return the last pair in a list
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;; 将 x 的 last-pair (cons a (cons b (cons c (... (cons n nil)))) 中的 (cons n nil) 连接到 y
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)

(define w (append! x y))
w
(cdr x)

;; What are the missing ⟨response⟩s? Draw box-and-pointer diagrams to explain your answer.
;; 1. (b) cause append would not change x
;; 2. (b c d) append! will modify x
;; skip the drawing part, so annoying

