#lang sicp

;; 常数空间求环

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

;; holy! circlur linked list!
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle '(a b c)))

;; 快慢指针
(define (is-cycle? x)
  (let ([fast (cdr x)] [slow x])
    (define (iter)
      (cond
        [(eq? fast slow) #t]
        [(or (null? fast) (null? (cdr fast))) #f]
        [else
         (begin
           (set! fast (cddr fast))
           (set! slow (cdr slow))
           (iter))]))
    (iter)))

(is-cycle? '(a b c))
(is-cycle? z)
