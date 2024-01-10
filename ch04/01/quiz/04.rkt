#lang sicp

(#%require "../evaluator/utils.rkt")
(#%require "../evaluator/special-forms.rkt")
(#%require "../evaluator/application.rkt")

;; 其实也可以用 application 里的 first-operand / rest-operands 来实现
(define (first-exp seq)
  (car seq))
(define (rest-exps seq)
  (cdr seq))

;; 原始实现
(define (and? exp)
  (tagged-list? exp 'and))
(define (and-actions exp)
  (cadr exp))

(define (eval-and exps env)
  (if (no-operands? exps)
      true
      (let ([first-value (eval (first-exp exps) env)])
        (if first-value
            (eval-and (rest-exps exps) env)
            first-value))))

(define (or? exp)
  (tagged-list? exp 'or))
(define (or-actions exp)
  (cadr exp))
(define (eval-or exps env)
  (if (no-operands? exps)
      false
      (let ([first-value (eval (first-exp exps) env)])
        (if first-value
            first-value
            (eval-or (rest-exps exps) env)))))

;; 派生实现
;; and 也可以看作是递归的 if
(define (derived-eval-and exps env)
  (if (no-operands? exps) true)
  (make-if (first-exp exps)
           (derived-eval-and (rest-exps exps) env)
           false))


(define (derived-eval-or exps env)
  (if (no-operands? exps) false)
  (make-if (first-exp exps)
           true
           (derived-eval-or (rest-exps exps) env)))
