#lang sicp

(define (tagged-list? exp tag)
  (if (pair? exp) (eq? (car exp) tag) false))

;; check if the key was set in records
(define (assoc key records)
  (cond
    [(null? records) #f]
    [(equal? key (caar records)) (car records)]
    [else (assoc key (cdr records))]))

;; 处理表达式列表
(define (first-exp seq)
  (car seq))
(define (rest-exps seq)
  (cdr seq))
(define (no-exps seq)
  (null? seq))
(define (last-exp? seq)
  (null? (cdr seq)))

(define (eval-sequence exps env)
  (cond
    [(last-exp? exps) (eval (first-exp exps) env)]
    [else
     (eval (first-exp exps) env)
     (eval-sequence (rest-exps exps) env)]))

(#%provide tagged-list?
           first-exp
           no-exps
           rest-exps
           eval-sequence
           assoc)
