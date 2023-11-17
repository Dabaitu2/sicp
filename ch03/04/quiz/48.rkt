#lang sicp
(#%require
 "../01-nature-of-time-in-concurrent-systems/serializer.rkt")

;; modified make-account with number
(define (make-account balance number)
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
        [(eq? m 'balanace) balance]
        [(eq? m 'number) number]
        [else (error "Unknown request: MAKE-ACCOUNT" m)]))
    dispatch))

(define (exchange account1 account2)
  (let ([difference (- (account1 'balance)
                       (account2 'balance))])
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;; in our modified serialized-exchange
;; we always try to access serializer of those account which has smaller number
;; so if a1<->a2 and a2<->a1 , while a1.num < a2.num
;; both exchange will try to access a1, and that would not make deadlock happen
(define (serialized-exchange account1 account2)
  (let ([serializer1 (account1 'serializer)]
        [serializer2 (account2 'serializer)]
        [account1Num (account1 'number)]
        [account2Num (account2 'number)])
    (if (> account1Num account2Num)
        ((serializer1 (serializer2 exchange)) account1
                                              account2)
        ((serializer2 (serializer1 exchange)) account1
                                              account2))))
