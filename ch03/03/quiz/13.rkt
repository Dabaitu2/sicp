#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;; holy! circlur linked list!
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle '(a b c)))
;; invoke below procedure will leads to infinite recursion
#| (last-pair z) |# 
;; still skipping drawing part, sorry
