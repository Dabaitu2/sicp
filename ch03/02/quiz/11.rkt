#lang racket

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
  (define (dispatch m)
    (cond
      [(eq? m 'withdraw) withdraw]
      [(eq? m 'deposit) deposit]
      [else (error "Unknown request: MAKE-ACCOUNT" m)]))
  dispatch)

(define acc (make-account 50))
((acc 'deposit) 40)
#| 90 |#
((acc 'withdraw) 60)
#| 30 |#


(define acc2 (make-account 100))


;;
;;
;;                  ┌────────────────────────────────────────────────────────────────────────────────────────────────────┐
;;                  │                                                                                                    │
;;                  │  make-account                                        acc                                           │
;;    global-env    │    │                                                   │                                           │
;;                  │    │     ▲                                             │                                           │
;;                  └────┼─────┬─────────────────────▲───────────────────────┼───────────────────────────────────────────┘
;;                       │     │                     │                       │
;;                       ▼     │              ┌──────┴──────────────┐        │
;;                             │              │ E1                  │        └─────────────┐
;;                   params: balance          │ balance: 50=>90=>30 │                      │
;;                   body:                    │                     │                      │
;;                   (define (withdraw ..) ┌──┼─withdraw            ◄───────────────┬──────┼─────────┬────────────────────┐
;;                    ...                  │  │                     │               │      │         │  m: deposit        │
;;                   dispatch              │  │ deposit ────────────┼─┐             │      │         │  E2                │
;;                                         │  │                     │ │             │      │         └────────────────────┘
;;                                         │  │ dispatch────────────┼─┼──────────▲  │      │
;;                                         │  │                     │ │          │  │      │              ┌─────────────────┐
;;                                         │  │                     ├─┼────┐     │  │      │              │ amount: 40      │
;;                                         │  └─────────────────────┘ │    │     ├──┼──────┼──────────────┼ E3              │
;;                                         │   ▲                      │    │     │  │      │              └─────────────────┘
;;                                         ▼   │                      │    │     │  │      │
;;                                             └──────────────────────┼────┼─────┼──┼──────┼────────────────────┬─────────────────────┐
;;                                      params: amount                │    │     │  │      │                    │                     │
;;                                      body:                         │    │     │  │      │                    │ amount: 60          │
;;                                      (if (>= balance) amount)      │    │     │  │      │                    │ E4                  │
;;                                        ...                         │    │     │  │      │                    │                     │
;;                                        "Insufficient funds")       │    │     │  │      │                    └─────────────────────┘
;;                                                                    │    │     │  │      │                    
;;                                                                    │    │     │  │      │
;;                                                                    ▼    │     │  │      │
;;                                                               params: amount  │  │      │
;;                                                               body：          │  │      │
;;                                                              （(set! balance .│  │      │
;;                                                                 balance)      │  │      │
;;                                                                               │  │      │
;;                                                                               ▼  │      │
;;                                                                            params│ m  ◄─┘
;;                                                                            body：
;;                                                                            (cond ((eq? m ...) withdraw)
;;                                                                               ...)
;;
;;
;;  1. define make-count, 创建环境对象，绑定到全局
;;  2. 执行 (make-account 50) 产生新环境 E1, balance 绑定实际参数 50, 创建三个 binding 分别指向三个新建的环境对象, 返回 dispatch 对应的环境对象 被 define 绑定到全局的 acc
;;  3. 执行 (acc 'deposit) 产生新环境 E2，环境指向 dispatch 的环境对象指向的环境 E1,  m 绑定实际参数 'deposit, 返回 deposit 对应的环境对象
;;  4. 执行 (deposit 40) 产生新环境 E3, 环境指向 deposit 的环境对象指向的环境 E1, amount 绑定实际参数 40, set! 将 balance 调整为 90
;;  5. 执行 (withdraw 60) 产生新环境 E4, 环境指向 withdraw 的环境对象指向的环境 E1, amount 绑定实际参数 60, set! 将 balance 调整为 30
;; 
;;  如果我们重新调用make-account, E1 环境创建一个全新的, 从而保证两个 account 的 distinct
;;  两个账号仅仅共享 global env
