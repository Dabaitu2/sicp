#lang racket

;; 修改版的 make-account
;; 执行操作前先校验密码

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (complaint . args)
    "Incorrect password")

  (define (dispatch input m)
    (if (eq? password input)
        (cond
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else
           (error "Unknown request -- MAKE-ACCOUNT" m)])
        complaint))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
