#lang sicp
(#%require "common.rkt")
;; sicp-1-03 用高阶函数做抽象
;; 计算给定范围的整数之和
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
;; 计算给定范围的立方和
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
;; 计算一个求π/8的收敛级数
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
;; 抽象出共同部分
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc x) (+ x 1))
(define (new-sum-cubes a b)
  (sum cube a inc b))
(new-sum-cubes 5 10)
(sum-cubes 5 10)
(define (identity x) x)
(define (new-sum-integers a b)
  (sum identity a inc b))
(new-sum-integers 1 5)

(define (new-pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
(new-pi-sum 1 1000)
(pi-sum 1 1000)

;; 求定积分
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.00001)
;; simpsom 积分法
(define (simpsom-intergral f a b n counter)
  (define (h) (/ (- b a) n))
  (define (co) (/ (h) 3))
  (define (get-y x)
    (f (+ a (* x (h)))))
  (define (term-simpsom x)
    (cond ((or (= x 0) (= x n)) (* (co) (get-y x)))
          ((even? x) (* (co) 2 (get-y x)))
          (else (* (co) 4 (get-y x)))))
  (cond ((not (even? n)) (display "wrong n"))
        (else (sum term-simpsom counter inc n))))
(simpsom-intergral cube 0 1 1000 0)

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
;; (fixed-point cos 1.0)
(define (golden-equation x)
  (+ 1 (/ 1 x)))
(fixed-point golden-equation 1.0)

;; 平均阻尼函数（防止猜测值过分震荡,帮助快速收敛）
;; 高阶函数: 参数是一个函数,返回也是一个函数
(define (average-damp f)
  (lambda (x)
    (average x 
             (f x))))
;; 求x^x = 1000 的一个根(x->log(1000)/log(x)的一个不动点)
(define formula 
  (lambda (x)
    (/ (log 1000) 
       (log x))))

(fixed-point formula 2.0)
(fixed-point (average-damp formula) 2.0)

;; 无穷连分式k项求和
(define (cont-frac fn fd k)
    (define (iter counter result)
      (cond ((= 1 counter) (/ (fn counter) (+ (fd counter) result)))
            (else (iter (- counter 1) (/ (fn counter) (+ (fd counter) result))))))
    (iter k 0))
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1000)

;; 求e
(define (e k)
  (define (N i) 1.0)
  (define (D i)
    (if (= 0 (remainder (+ i 1) 3))
        (* 2 (/ (+ i 1) 3))
        1))
  (+ 2.0 (cont-frac N D k)))
(e 1000)

;; lambert公式计算正切函数值(弧度制)
(define (tan-cf x k)
 (define (N i)
   (if (= i 1.0)
       x
       (- 0 (square x))))
  (define (D i)
    (- (* 2 i) 1.0))
  (cont-frac N D k))
(tan-cf 45 100)


;; 求导数
((deriv cube 0.00001) 5)

;; 牛顿法求不动点
;; 牛顿变换
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g 0.00001) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))
(sqrt 100)

;; 更高一级抽象的求转换后不动点
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; 求x^3+ax^2+bx+c的零点
(define (cubic a b c)
  (lambda (x)
          (+ (cube x)
             (* a (square x))
             (* b x)
             c)))
(define (get-zero-of-cubic a b c)
  (newtons-method (cubic a b c) 1))
(get-zero-of-cubic 2 2 2)
