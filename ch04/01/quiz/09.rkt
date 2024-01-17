#lang racket

(#%require "../evaluator/special-forms.rkt")
(#%require "../evaluator/evaln.rkt")
(#%require "../evaluator/utils.rkt")

;; (while <predicate> <body>)
;; (while (< i 10)
;;        (display i)
;;        (set i (+ i 1)))

(define (while? exp)
  (tagged-list? exp 'while))
(define (make-while predicate body)
  (cons 'while (cons predicate body)))
(define (while-clauses exp)
  (cdr exp))
(define (while-predicate clause)
  (car clause))
(define (while-body clause)
  (cdr clause))

(define (while->combination exp)
  (let ([predicate (while-predicate exp)]
        [body (while-body exp)])
    (make-if predicate
             ;; 这里的 body 是一个 list，而 if 接受的实际上是单个表达式，所以需要用 begin 包装一下
             ;; 这里不用 apply，是因为 if 的 alternative / consequence 求值是运行时，不是编译时，所以这里就是一个表达式
             (sequence->exp (list body exp))
             'done)))

(define (eval-while exp env)
  (eval (while->combination exp) env))
