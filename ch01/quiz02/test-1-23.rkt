#lang sicp

(define (prime? n)
  (define (smallest-divisor n)
    (define (square x) (* x x))
    (define (divides? a b) (= (remainder b a) 0))
    (define (find-divisor test-divisor)
      ;; 跳过所有偶数
      ;; 改进寻找素数的方案，因为 divisor 中不需要检查 > 2 的任何偶数，
      ;; 因为如果能整除这些偶数
      ;; 肯定可以 整除 2，反之如果 2 不可以被整除, 那么这些偶数就都不需要整除了。
      (define (next n)
        (if (= (remainder n 2) 0)
            (+ n 1)
            (+ n 2)))
      (cond [(> (square test-divisor) n) n]
            [(divides? test-divisor n) test-divisor]
            [else (find-divisor (next test-divisor))]))
    (find-divisor 2))
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (display " ms ")
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

(search-for-primes 1000 1019)
(search-for-primes 10000 10037)
(search-for-primes 100000 100043)
(search-for-primes 1000000 1000037)


