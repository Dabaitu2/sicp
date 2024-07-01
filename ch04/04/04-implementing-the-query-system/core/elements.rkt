#lang sicp
(#%require "../utils.rkt")

(define (rule? statment)
  (tagged-list? statment 'rule))
(define (conclusion rule)
  (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule)) '(always-true) (caddr rule)))

(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter)))
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(#%provide rule?
           conclusion
           rule-body
           make-new-variable
           new-rule-application-id)
