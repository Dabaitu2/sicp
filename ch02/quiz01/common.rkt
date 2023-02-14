#lang sicp

(define
  (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (average x y)
  (/ (+ x y) 2))
(define (divides? a b)
  (= (remainder b a) 0))
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(define (even? n)
  (= (remainder n 2) 0))
(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
(define (factorial n)
  (fact-iter 1 1 n))
(define (sum term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
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
  (fast-find-divisor n 2))
;; 一个数的最小因子是他自己(除了1之外)
(define (prime? n)
  (= n (fast-smallest-divisor n)))

;; 求GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; 导函数
(define (deriv g dx)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

;; 函数组合
(define (compose f g)
  (lambda (x)
    (f (g x))))
;; 无穷连分式k项求和
(define (cont-frac fn fd k)
  (define (iter counter result)
    (cond ((= 1 counter) (/ (fn counter) (+ (fd counter) result)))
          (else (iter (- counter 1) (/ (fn counter) (+ (fd counter) result))))))
  (iter k 0))

;; 找出函数的不动点
(define tolerance 0.00001)
(define (display-info guess step)
  (display "Step: ")
  (display step)
  (display " ")

  (display "Guess: ")
  (display guess)
  (newline))
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (let ((next (f guess)))
      (display-info guess step)
      (if (close-enough? guess next)
          (display "\n")
          (try next (+ 1 step)))))
  (try first-guess 1))

(define (average-damp f)
  (lambda (x)
    (average x
             (f x))))

;; 函数的重复运用
(define (repeated f base)
  (define (iter counter result)
    (if (= 1 counter)
        result
        (iter (- counter 1) (compose f result)))
    )
  (iter base f))
((repeated square 2) 5)
;; n次平均阻尼函数
(define (multi-average-damp f n)
  (repeated (average-damp f) n))
;; 返回一个过程,这个过程接受一个参数,计算这个参数的n次方,并对结果做damp-time次平均阻尼处理
(define (damp-nth-root n damp-time)
  (lambda (x)
    (fixed-point
     (multi-average-damp
      (lambda (y)
        (/ x (expt y (- n 1))))
      damp-time)
     1.0)))
;; 实验证明需要的damp次数为[lgn](对logn取整)
(define (lg n)
  (cond ((> (/ n 2) 1) (+ 1 (lg (/ n 2)))) ;; 这一步的理由是, n>2 说明 n/2 > 1,logn = log(n/2) + log(2) log(2) = 1 故可以这样递归的求解log(n/2)
        ((< (/ n 2) 1) 0)
        (else 1)))
(define (nth-root n)
  (damp-nth-root n (lg n)))

(#%provide square average even? expt factorial cube sum prime? divides? gcd deriv compose cont-frac fixed-point repeated nth-root)
