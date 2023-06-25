#lang racket
(#%require
 "../../../common/data/conventional-interface.rkt")

;; 提供公用账户能力

(define (make-account balance password)
  (let ([pwd-list (list password)])
    (define (withdraw amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (make-joint new-pwd)
      (set! pwd-list (append pwd-list (list new-pwd)))
      dispatch)

    (define (complaint . args)
      (error "Incorrect password"))

    (define (dispatch input m)
      (if (> (length (filter (lambda (pwd) (eq? input pwd))
                             pwd-list))
             0)
          (cond
            [(eq? m 'withdraw) withdraw]
            [(eq? m 'deposit) deposit]
            [(eq? m 'joint) make-joint]
            [else
             (error "Unknown request -- MAKE-ACCOUNT" m)])
          complaint))
    dispatch))

(define (make-joint old-account old-pwd new-pwd)
  ((old-account old-pwd 'joint) new-pwd))

(define peter-acc (make-account 100 'open-sesame))
;; 为原有账户创建新的访问途径, 通过新名字 新密码也能执行操作
(define paul-acc
  (make-joint peter-acc 'open-sesame 'roesbud))

((peter-acc 'open-sesame 'withdraw) 40)
((paul-acc 'roesbud 'deposit) 50)
