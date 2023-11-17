#lang racket
(#%require "../../../common/math/num-theory.rkt")
(#%require
 "../../../common/data/conventional-interface.rkt")

;; 迭代风格的求素数和
(define (sum-primes a b)
  (define (iter count accum)
    (cond
      [(> count b) accum]
      [(prime? count (iter (+ count 1) (+ count accum)))]
      [else (iter (+ count 1) accum)]))
  (iter a 0))

;; 基于 sequence 的求素数和
;; 在代码理解上，这样的方式确实降低了成本
;; 然而代码调用的中间带宽需求是很高的
;; 我们需要用 enumerate-interval 构造一整个 list 再传给 filter，再全部 filter 掉之后形成新的 interval
;; 这中间的每一个操作都是必须要批处理完才可以进行下一步的
;; 相较于前面的迭代操作，这样的操作无法中断，可能会变得非常低效
(define (sum-primes-2 a b)
  (accumulate + 0 (filter prime? (enumerate-interval a b))))


;; 比如针对这个过程，如果我们给出的区间范围特别大，例如 1000, 1000000
;; 这样的方式就相当低效率了
;; 因为我们实际上只需要整个区间中的前两个素数
(define (get-second-prime-of-interval a b)
  (car (cdr (filter prime? (enumerate-interval a b)))))


(get-second-prime-of-interval 10000 100000)

