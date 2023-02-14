
#lang sicp

(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

;; 1.2 过程与他们产生的计算
;; a. 使用“计算产生的形状”占用的资源, 使用递归和迭代做例子
;; b. 使用 Order of growth 增长阶 来评估计算资源损耗 (本质上就是复杂度)
;;
;; 1.2.1 procedure and iteration
;; 递归改迭代的关键
;; 1. 系统支持尾递归
;; 2. 递归必须不能有别的东西组合，只能是一个单独的过程
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial2 n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;; 1.2.2 Tree Recursion
;; 树形递归改迭代的关键
;; 1. 上面的
;; 2. 由于树形递归会存在表达式组合，所以需要把表达式移到参数里面去
;; 规避此问题
(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib (- n 1))
                 (fib (- n 2)))]))

;; 迭代方案，利用尾递归
(define (fib2 n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;; 实例: 找零钱问题
;; 此题本质上是 dp 问题
;; 下面是一个暴力递归解法
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

;; it's like a table
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; (count-change 100)


;; 快速幂 递归方案
;; 1. b^n = b * (b * (b * (b * (...))
;; 2. 如果 n 是偶数 b^n = (b^(n/2))^2
;;    如果 n 是奇数 b^n = b * b^n-1
(define (fast-expt b n)
  (define (square x) (* x x))
  (cond [(= n 0) 1]
        [(even? n) (square (fast-expt b (/ n 2)))]
        [else (* b (fast-expt b (- n 1)))]))


;; 1.2.5 GCD 求最大公约数
;; 求GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b)))) ;; remainder 是求余
;; (gcd 100 20)

;; 1.2.6 素数检测
;; 最为朴素的算法: 一个数是素数当且仅当此数的最小因数 = 自身
;; 因此我们从 2 开始寻找直到 根号n

(define (smallest-divisor n)
  (define (square x) (* x x))
  (define (divides? a b) (= (remainder b a) 0))
  (define (find-divisor test-divisor)
    (cond [(> (square test-divisor) n) n]
          [(divides? test-divisor n) test-divisor]
          [else (find-divisor (+ test-divisor 1))]))
  (find-divisor 2))

;; (smallest-divisor 19999)

(define (prime? n)
  (= n (smallest-divisor n)))

;; 费马小定理做概率性的检查
;; 费马小定理其实并不正确，有一类数叫 Carmichael 数就可以骗过费马检查
;; 比如 561
;; 首先需要求一个数 base 的 exp 次方 module m 的余数
;; 这里还借助了 快速幂 的计算方法
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

;; 应用费马检查判断质数
;; 迭代法费马检查, time 减小到 0 认为没有问题, 否则认为 不是质数
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(#%provide
 cube
 average
 square
 prime?)
