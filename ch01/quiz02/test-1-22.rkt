#lang sicp

(define (prime? n)
  (define (smallest-divisor n)
    (define (square x) (* x x))
    (define (divides? a b) (= (remainder b a) 0))
    (define (find-divisor test-divisor)
      (cond [(> (square test-divisor) n) n]
            [(divides? test-divisor n) test-divisor]
            [else (find-divisor (+ test-divisor 1))]))
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


