#lang racket

;; 在这里
;; 我们引入了新的语法
;; 1. (set! <name> <new-value>) 用于改变某个变量的值, 改变变量 / 数据结构的操作都以 ! 结尾, 是一种约定
;; 2. (begain <exp1> <exp2> ... <expk>) 用于按照顺序求内部多个 expression 的结果, 非常方便用来和 if 做搭配, 其实我们之前用过的 cond 里的 consequence 就可以是一个序列

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))

;; 但上面的代码还是存在问题，balance 是一个全局变量，它很容易的可以被其他逻辑用 set! 篡改
;; 因此我们需要将状态私有化

(define (new-withdraw)
  (let ([balance 100])
    (lambda (amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))))

;; 然而，上面这种模式就证明我们之前所提出的代换 (substitution) 模型是有问题的了
;; 如果是简单的代换模型，则每次使用返回的函数时，都会被代换成定义处，那么定义的值也会被按定义代换成 100
;;
;; 然而在这里返回的函数多次调用后居然可以影响定义函数时所创建的 balance
;; balance 会随着调用而变化，状态会保存, 使用的时候会读取“当前值” 而不是每次调用对应的 balance 为 100
;; 这意味着函数执行的时候还必须有一个 "环境" 的概念。用简单的代换模型是没有环境一说的
;; 这里就可以进一步的理解，为什么说 js 很大程度上借鉴了 lisp, 这种闭包写法可见一斑

;; 我们甚至可以捕获外部函数的参数, 因为形式参数本来就是 local 的
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        ("Insufficient funds"))))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(W1 50)
(W2 70)

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
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 100))

((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 40)
((acc 'withdraw) 60)

;; 多次调用 make-account 会创造不同的局部状态, 从而实现对对象的初步模拟
(define acc2 (make-account 100))


