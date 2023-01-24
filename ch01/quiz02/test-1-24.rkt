#lang sicp

;; 费马小定理做概率性的检查
;; 费马小定理其实并不正确，有一类数叫 Carmichael 数就可以骗过费马检查
;; 比如 561
;; 首先需要求一个数 base 的 exp 次方 module m 的余数
;; 这里还借助了 快速幂 的计算方法
(define (expmod base exp m)
  (define (square x)
    (* x x))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        ;; 不能除 2 就正常转化为 (x*x^n-1)modm
        (else (remainder (* base (expmod base (- exp 1) m)) m))
        ))

;; 费马检查
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; 应用费马检查判断质数
;; 迭代法费马检查, time 减小到 0 认为没有问题, 否则认为 不是质数
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  )

;; 找到大于某个数的最小素数
;; 首先这个数肯定不可能是除 2 之外的偶数
(define (search-for-primes lower upper)
  (define (iter n)
    (cond [(<= n upper) (timed-prime-test n) (iter (+ n 2))]))

  (iter (if (odd? lower)
            lower
            (+ lower 1))
        )
  )

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

(newline)
(timed-prime-test 1000000007)
(timed-prime-test 1000000009)
(timed-prime-test 1000000021)
