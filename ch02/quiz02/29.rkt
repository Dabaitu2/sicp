#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; 树的遍历
(define (total-weight mobile)
  (cond
    [(null? mobile) 0]
    [(not (pair? mobile)) mobile]
    [else
     (+ (total-weight (branch-structure
                       (left-branch mobile)))
        (total-weight (branch-structure
                       (right-branch mobile))))]))

(define m
  (make-mobile
   (make-branch 1 2)
   (make-branch 2
                (make-mobile (make-branch 1 3)
                             (make-branch 1 4)))))
(define m2
  (make-mobile (make-branch 2 3) (make-branch 2 3)))

(total-weight m)
(total-weight m2)

;; 树的力矩平衡判断
;; 力矩 = length * weight
(define (balance? mobile)
  (define (torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
  (if (not (pair? mobile))
      true
      (and (= (torque (left-branch mobile))
              (torque (right-branch mobile)))
           (balance? (branch-structure
                      (left-branch mobile)))
           (balance? (branch-structure
                      (right-branch mobile))))))

(balance? m)
(balance? m2)
