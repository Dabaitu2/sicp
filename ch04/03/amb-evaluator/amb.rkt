#lang sicp

(#%require "./utils.rkt")

(define (amb? exp)
  (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))

(#%provide amb? amb-choices)
