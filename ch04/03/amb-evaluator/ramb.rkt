#lang sicp

(#%require "./utils.rkt")

(define (ramb? exp)
  (tagged-list? exp 'ramb))

(define (ramb-choices exp) (cdr exp))

(#%provide ramb? ramb-choices)
