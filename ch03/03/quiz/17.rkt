#lang sicp

(define (memq? item x)
  (cond
    [(null? x) false]
    [(eq? item (car x)) x]
    [else (memq? item (cdr x))]))

;; correct way to count the number of pairs in any list structure
;; we need to maintain an extra data list structure to remember the cons we have counted
(define (count-pairs x)
  (let ([counted-list '()])
    (define (helper x)
      (if (or (not (pair? x)) (memq? x counted-list))
          0
          (begin
            (set! counted-list (cons x counted-list))
            (+ (helper (car x)) (helper (cdr x)) 1))))
    (helper x)))

(define x (cons 'foo '()))
(define y (cons x x))
(define a (cons y '()))
(count-pairs a)
