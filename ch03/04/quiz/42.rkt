#lang sicp
(#%require "../01-nature-of-time-in-concurrent-systems/serializer.rkt")

;; I just feel like it's safe to do so
;; cause according to the definition, all the others procedure have to wait until running procdure finishing
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
    (let ([protected-withdraw (protected withdraw)]
          [protected-deposit (protected deposit)])
      (define (dispatch m)
        (cond
          [(eq? m 'withdraw) protected-withdraw]
          [(eq? m 'deposit) protected-deposit]
          [(eq? m 'balance) balance]
          [else (error "Unknown request: MAKE-ACCOUNT" m)]))
      dispatch)))
