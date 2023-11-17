#lang racket

;; 基于概率 (米勒拉宾检查) 的快速素数算法
;; 费马小定理做概率性的检查
;; 费马小定理其实并不正确，有一类数叫 Carmichael 数就可以骗过费马检查
;; 比如 561
;; 首先需要求一个数 base 的 exp 次方 module m 的余数
;; 这里还借助了 快速幂 的计算方法
(define (square x)
  (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        ;; 不能除 2 就正常转化为 (x*x^n-1)modm
        (else (remainder (* base (expmod base (- exp 1) m)) m))
        ))
;; (trace expmod)
;; (expmod 3 5 2)

;; 费马检查
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


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


;; 应用费马检查判断质数
;; 迭代法费马检查, time 减小到 0 认为没有问题, 否则认为 不是质数
(define (fermat-fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fermat-fast-prime? n (- times 1)))
        (else #f)))


;; 应用米勒拉宾检查判断质数
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fermat-fast-prime? n (- times 1)))
        (else #f)))

;; 费马检查给出了错误的判断
;; (fermat-fast-prime? 561 100)
;; 米勒拉宾检查能够检测出卡尔迈克数
;; (fast-prime? 561 100)

(#%provide fast-prime? fermat-fast-prime?)
