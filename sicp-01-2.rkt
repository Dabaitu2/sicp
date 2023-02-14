#lang sicp
(#%require "common.rkt")
(#%require racket/trace)

;; 求GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(gcd 100 20)


;; 判断素数(增长阶为O(logn))
(define (square n)
           (* n n))
(define (divides? a b)
  (= (remainder b a) 0))
;; 这个函数寻找从某个数n从test-divisor开始有没有因子
;; 如果一个数不是素数,那它一定有<=√n 的因子
(define (find-divisor n test-divisor)
  ;; (如果test-divisor的平方>n, 则预设自己是自己的因子，比如对于2这个test-divisor, 小于4的1,2,3肯定都是没有其他因子的
  ;; 这个地方预设了test-divisor是从最小的开始的，它只能检测到√n之前的因子（但只要√n前有因子，√n后肯定就有）
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
;; 从2开始就可以寻找到最小因子
(define (smallest-divisor n)
  (find-divisor n 2))
;; 一个数的最小因子是他自己(除了1之外)
(define (prime? n)
  (= n (smallest-divisor n)))
(prime? 23)
(find-divisor 12 5)



;; 一个数的幂对另一个数取模
;; 对任意的x,y,m都有 (x*y)modm = ((xmodm)*(ymodm))modm
;; 又因为对于任意的xmodm, xmodm = (xmodm)modm
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        ;; 不能除2就正常转化为 (x*x^n-1)modm
        (else (remainder (* base (expmod base (- exp 1) m)) m))
        ))
;; (trace expmod)
(expmod 3 5 2)



;; 费马检查
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
;; 迭代法费马检查, time减小到0认为没有问题, 否则认为不是质数
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))
(smallest-divisor 19999)




;; 使用runtime计算寻找素数的时间
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (report-prime elapsed-time)
  (display " yes")
  (display " *** ")
  (display elapsed-time)
  (display "ms"))
(define (get-next-odd n)
  (if (= 0 (remainder n 2))
      (+ n 1)
      (+ n 2)))
(define (search-for-primes n start-time elapsed-time counter)
  (cond ((= counter 0)
         (display "时间过去了: ")
         (display elapsed-time)
         (display "μs\n") 
         )
        ((prime? n)
         (display n)
         (display "\n")
         (search-for-primes (get-next-odd n) (runtime) (+ elapsed-time (- (runtime) start-time)) (- counter 1))
         )
        (else (search-for-primes (get-next-odd n) (runtime) (+ elapsed-time (- (runtime) start-time)) counter))))
(search-for-primes 100000 (runtime) 0 3)


;; 改进的寻找素数
(define (fast-find-divisor n test-divisor)
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  ;; (如果test-divisor的平方>n, 则预设自己是自己的因子,比如对于2这个test-divisor, 小于4的1,2,3肯定都是没有其他因子的
  ;; 这个地方预设了test-divisor是从最小的开始的,它只能检测到√n之前的因子(但只要√n前有因子,√n后肯定就有)
  ;; 如果一个数不能被2整除,那它肯定不是偶数,就不可能被接下来的任何偶数整除 
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (fast-find-divisor n (next test-divisor)))))

;; 从2开始就可以寻找到最小因子
(define (fast-smallest-divisor n)
  (find-divisor n 2))
;; 一个数的最小因子是他自己(除了1之外)
(define (better-prime? n)
  (= n (fast-smallest-divisor n)))
(define (search-for-primes-fast n start-time elapsed-time counter)
  (cond ((= counter 0)
         (display "时间过去了: ")
         (display elapsed-time)
         (display "μs \n")
         )
        ((fast-prime? n)
         (display n)
         (display "\n")
         (search-for-primes-fast (get-next-odd n) (runtime) (+ elapsed-time (- (runtime) start-time)) (- counter 1))
         )
        (else (search-for-primes-fast (get-next-odd n) (runtime) (+ elapsed-time (- (runtime) start-time)) counter))))
(search-for-primes 1000 (runtime) 0 3)
(search-for-primes 1000000 (runtime) 0 3)


;; 检测个Carmichael数字
(define (try-it a value-be-test)
    (= (expmod a value-be-test value-be-test) a))
(define (test-iter a n)
  (cond ((= a 1) #t)
        ((try-it a n) (test-iter (- a 1) n))
        (else #f)))
(define (Carmichael-test n)
  (test-iter (- n 1) n))
(Carmichael-test 1105)

;; 判断非平凡平方根
(define (nontrivial-square-root? a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= 1 (remainder (square a) n))))
(define (mr-expmod base exp m)
  (cond ((= exp 0) 1)
        ((nontrivial-square-root? base m) 0)
        ((even? exp)
         (remainder (square (mr-expmod base (/ exp 2) m))
                    m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))
        ))
(define (non-zero-random n)
    (let ((r (random n)))
        (if (not (= r 0))
            r
            (non-zero-random n))))
(define (mr-iter n times)
  (cond ((= times 0) #t)
        ((= (mr-expmod (non-zero-random n) (- n 1) n) 1)
         (mr-iter n (- times 1)))
        (else #f)))
(define (miller-rabin-test n)
  (mr-iter n (ceiling (/ n 2))))

(trace mr-iter)
(miller-rabin-test 11)
