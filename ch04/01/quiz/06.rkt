#lang sicp

;; achieve let expression by derived form

(#%require "../evaluator/utils.rkt")
(#%require "../evaluator/special-forms.rkt")

(define (let? exp)
  (tagged-list? exp 'let))
(define (let-clauses exp)
  (cdr exp))
(define (let-bindings clause)
  (car clause))
(define (let-vars bindings)
  (if (null? bindings)
      '()
      (cons (caar bindings)
            (let-vars (cdr bindings)))))
(define (let-values bindings)
  (if (null? bindings)
      '()
      ;; 所有的表达式默认都是 list 而非 cons 组成 
      (cons (cadar bindings)
            (let-values (cdr bindings)))))

(define (let-body clause)
  (cdr clause))

(define (let->combination exp)
  (let ([bindings (let-bindings exp)])
    (list (make-lambda (let-vars bindings)
                       (let-body exp))
          (let-values bindings))))

(define (eval-let exp env)
  (eval (let->combination (let-clauses exp)) env))
