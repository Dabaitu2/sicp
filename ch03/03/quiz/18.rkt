#lang sicp

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

;; holy! circlur linked list!
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle '(a b c)))

(define (memq? item x)
  (cond
    [(null? x) false]
    [(eq? item (car x)) x]
    [else (memq? item (cdr x))]))

;; 检测环, 本质上还是递归逻辑，但是递归外面有个状态记录, 通过环境能够 keep track of that list
(define (is-cycle? x)
  (let ([counted-list '()])
    (define (helper x)
      (cond
        [(or (null? x) (not (pair? x))) #f]
        [(memq? x counted-list) #t]
        [else
         (begin
           (set! counted-list (cons x counted-list))
           (or (helper (car x)) (helper (cdr x))))]))
    (helper x)))

(is-cycle? z)
(define y '(a b c))
(is-cycle? y)
