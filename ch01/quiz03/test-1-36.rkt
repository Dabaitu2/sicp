#lang sicp
(#%require "../02.rkt")

(define tolerance 0.00001)
(define (display-info guess step)
  (display "Step: ")
  (display step)
  (display " ")

  (display "Guess: ")
  (display guess)
  (newline))

;; 求不动点
;; 通过不停执行函数 f(f(f(x))) 的方式，找到变化不大的那个点
;; 就是不动点，通过不停的计算猜测来比较
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (let ([next (f guess)])
      (display-info guess step)
      (if (close-enough? guess next)
          (display "\n")
          (try next (+ 1 step)))))
  (try first-guess 1))

;; 平均阻尼函数（防止猜测值过分震荡,帮助快速收敛）
;; 高阶函数: 参数是一个函数,返回也是一个函数
(define (average-damp f)
  (lambda (x)
    (average x
             (f x))))
;; 求x^x = 1000 的一个根(x->log(1000)/log(x)的一个不动点)
(define (formula x)
  (/ (log 1000)
     (log x)))

(display "without average-damp")
(newline)
(fixed-point formula 2.0)

(display "with average-damp")
(newline)
(fixed-point (average-damp formula) 2.0)

