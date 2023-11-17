#lang sicp
(#%require "./serializer.rkt")

;; suppose we have such procedure which can swap the balances in two bank accounts
;; this procedure will work fine under one process cuz all the procedure has been put into one serializor
;; and they can keep in sensible order
(define (exchange account1 account2)
  (let ([difference (- (account1 'balance)
                       (account2 'balance))])
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;; but when we consider on the situation that multiple process work parallelly
;; the validity of the output then can not be guaranteed
;; for example, while someone wanna change a1<->a2, while another guy wanna change a2<->a3
;; then if a1<->a2 has finished between a2<->a3's read and write, then the answer would not correct anymore!

;; a way to achieve this is to make the whole exchange procedure serialized by serializers of both accounts
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
        [(eq? m 'withdraw) withdraw]
        [(eq? m 'deposit) deposit]
        [(eq? m 'balance) balance]
        [(eq? m 'serializer) balance-serializer]
        [else (error "Unknown request -- MAKE-ACCOUNT" m)]))
    dispatch))

;; now, task of maintaining the validity is handed over to accounts
(define (deposit account amount)
  (let ([s (account 'serializer)] [d (account 'deposit)])
    ((s d) amount)))

;; and we can use the same way to serialize the exchange procedure
;; put exchange into serializer2, we can guaranteen its execution cannot be broken by any account2 procedure
;; (a2 deposit / withdraw need wait the exchange procedure finished)
;; and put the serialized procedure into serializer1 can make sure any step happened under account1 would not stop exchange procedure
;; (a1 deposit / withdraw need wait the internal serialized func finished)
(define (serialized-exchange account1 account2)
  (let ([serializer1 (account1 'serializer)]
        [serializer2 (account2 'serializer)])
    ((serializer1 (serializer2 exchange)) account1
                                          account2)))

(#%provide serialized-exchange
           make-account-and-serializer
           deposit)
