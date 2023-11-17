#lang sicp
(#%require "../01-nature-of-time-in-concurrent-systems/serializer.rkt")


;; belowing procedure is what louis suggest
;; he/she? think we should wrap internal procedure with the exported serializer to simplify other procedure
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ([balance-serializer (make-serializer)])
    (define (dispatch m)
      (cond
        [(eq? m 'withdraw) (balance-serializer withdraw)]
        [(eq? m 'deposit) (balance-serializer deposit)]
        [(eq? m 'balance) balance]
        [(eq? m 'serializer) balance-serializer]
        [else (error "Unknown request: MAKE-ACCOUNT" m)]))
    dispatch))

;; like this
(define (deposit account amount)
  ((account 'deposit) amount))

(define (withdraw account amount)
  ((account 'withdraw) amount))


;; but he/she was wrong!
;; considering about this serialized-exchange procedure
;; cuz we put the outter procedure (exchange) into the serializer while invoke it
;; and when executing on inner procedure like withdraw or deposit
;; they will be put into the serializer too, and they have to wait the outter procedure end!
;; while the outer procedure's finishing condition is the internal procedure end
;; that's a deadlock, and the waiting would never end!
(define (exchange account1 account2)
  (let ([difference (- (account1 'balance)
                       (account2 'balance))])
    (withdraw account1 difference)  ;; -> when this was invoke, it has to wait the outter procedure finished, that's the deadlock
    (deposit account2 difference)))

(define (serialized-exchange account1 account2)
  (let ([serializer1 (account1 'serializer)]
        [serializer2 (account2 'serializer)])
    ((serializer1 (serializer2 exchange)) account1  ;; -> invoke this will make the exchange into serializer
                                          account2)))
