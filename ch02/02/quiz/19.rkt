#lang sicp

;; 找零钱问题的 list 解法
(define (count-change amount)
  (cc amount 5))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 25 10 5 2 1 0.5))

(define (except-first-domination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))
(define (first-denomination coin-values)
  (car coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-domination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

(cc 100 us-coins) ;; 292 ways to count the change
(cc 10 us-coins) ;; 4 ways to count the change: 1*10, 5*2 10*1 1*5 + 5*1
(cc 15 us-coins) ;; 6 ways to count the change
