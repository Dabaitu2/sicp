#lang sicp

(#%require
 "../01-nature-of-time-in-concurrent-systems/serializer.rkt")

;; not neccessary, readonly operation is always legal even 
;; in concurrent situation
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ([protected (make-serializer)])
    (define (dispatch m)
      (cond
        [(eq? m 'withdraw) (protected withdraw)]
        [(eq? m 'deposit) (protected deposit)]
        [(eq? m 'balance)
         ((protected (lambda () balance)))] ; serialized
        [else (error "Unknown request: MAKE-ACCOUNT" m)]))
    dispatch))
