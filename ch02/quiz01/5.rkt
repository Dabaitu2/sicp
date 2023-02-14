#lang sicp

;; helper
(define (exp base n)
  (define (iter x result)
    (if (= x 0)
        result
        (iter (- x 1) (* base result))))
  (iter n 1))


;; 求出将目标整数反复除固定 divisor 直到 remainder 不为 0 时的 counter
;; 将反复除 divisor 转换为试探 divisor^c 中的 c 能有多大，以至于
;; n / divisor^c 的余数始终为 0
(define (count-0-remainder-divisions n divisor)
  (define (iter counter)
    (if (= 0 (remainder n (exp divisor counter)))
        (iter (+ counter 1))
        (- counter 1))) ;; 最后一下已经不满足条件了，所以要把 counter 还原回去
  (iter 1))

;; 基于目标实现的 cons , car , cdr
(define (cons a b) (* (exp 2 a) (exp 3 b)))
(define (car z)
  (count-0-remainder-divisions z 2))
(define (cdr z)
  (count-0-remainder-divisions z 3))

