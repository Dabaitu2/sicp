#lang racket

;; 输错超过 7 次就叫警察
(define (make-account balance password)
  (let ([wrong-count 0])
    (define (withdraw amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (call-the-cops)
      "I've call the cops! You wait here.")

    (define (complaint . args)
      (set! wrong-count (+ wrong-count 1))
      (if (>= wrong-count 7)
          (call-the-cops)
          "Incorrect password"))

    (define (reset-count m)
      (set! wrong-count 0)
      m)

    (define (dispatch input m)
      (if (eq? password input)
          (cond
            [(eq? m 'withdraw) (reset-count withdraw)]
            [(eq? m 'deposit) (reset-count deposit)]
            [else
             (error "Unknown request -- MAKE-ACCOUNT" m)])
          complaint))
    dispatch))


(define acc (make-account 100 'secret-password))

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)

((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
