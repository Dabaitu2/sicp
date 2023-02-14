#lang sicp

(define (square n)
  (* n n))


;; 米勒-拉宾检查 Miller-Rabin check
;; 它是费马小定理的一个变形
;; 判断非平凡平方根, 如果遇到了非平凡平方根，那么 n 一定不是素数
(define (nontrivial-square-root? a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= 1 (remainder (square a) n))))

;; Miller-Rabin 版 expmod
(define (mr-expmod base exp m)
  (cond [(= exp 0) 1]
        ;; 额外增加对非平凡平方根的检查
        [(nontrivial-square-root? base m) 0]
        [(even? exp) (remainder (square (mr-expmod base (/ exp 2) m)) m)]
        [else (remainder (* base (mr-expmod base (- exp 1) m)) m)]
        ))

;; 生成大于 0 小于 n 的随机数
;; 因为系统提供的 random 函数可以保证随机值小于 n
;; 因此我们自己的随机函数只要确保随机数不为 0 即可
(define (non-zero-random n)
  ;; 使用 let 缓存内部变量
  (let ((r (random n)))
    (if (not (= r 0))
        r
        (non-zero-random n))))

;; 米勒拉宾检查：
;; 如果 n 是素数，a 是小于 n 的整数, 则 a^(n-1) 与 1 模 n 同余
;; (【1 模 n】 = 1 因此可以直接拿去和 1. 比)
;; 另外，如果 n 是素数
;; 至少有一半的 a < n 按照此种情况计算会遇到 “非平凡平方根”
(define (miller-rabin-test n)
  ;; 使用改造过的 expmod 进行检查 
  (define (mr-iter n times)
    (cond ((= times 0) #t)
          ((= (mr-expmod (non-zero-random n) (- n 1) n) 1)
           (mr-iter n (- times 1)))
          (else #f)))
  ;; 检查 1/2 n 次即可
  (mr-iter n (ceiling (/ n 2))))

(miller-rabin-test 561)
