#lang sicp

(#%require "../utils.rkt")
(#%require "../evaln.rkt")
(#%require "../special-forms/if.rkt")

(define (unless? exp)
  (tagged-list? exp 'unless))

(define (unless-clause exp)
  (cdr exp))
(define (unless-condition clause)
  (car clause))
(define (unless-usual-value clause)
  (cadr clause))
(define (unless-exceptional-value clause)
  (caddr clause))

(define (unless->if exp)
  (let ([clause (unless-clause exp)])
    (make-if (unless-condition clause)
             (unless-exceptional-value clause)
             (unless-usual-value clause))))

(define (analyze-unless exp)
  (lambda (env) ((analyze (unless->if exp)) env)))

(#%provide analyze-unless)
