#lang sicp

(#%require "./02.rkt")

;; 1.3 高阶函数
;; 1.3.1 过程作为参数, 这就是一种高阶函数的感觉
(define (cube x)
  (* x x x))

;;; 计算 a-b 整数之和
(define (sum-integers a b)
  (if (> a b) 0 (+ a (sum-integers (+ a 1) b))))

;; 计算给定范围的立方和
(define (sum-cubes a b)
  (if (> a b) 0 (+ (cube a) (sum-cubes (+ a 1) b))))

;; 计算一个求π/8的收敛级数
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;; 抽象出共同部分,  这本质就抽象了数学中的 Sigma 函数
;; term 规定了如何计算需要被加入的每一个部分
;; next 决定了下一个部分怎么获得
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) ;; 对 a 做一些操作
         (sum term (next a) next b)))) ;; 此处仍然是递归

;; next -> +1, term -> cube
(define (inc x)
  (+ x 1))
(define (new-sum-cubes a b)
  (sum cube a inc b))

(new-sum-cubes 5 10)
(sum-cubes 5 10)

;; term -> identity 取得其本身
(define (identity x)
  x)
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
;; 定积分可以近似看作这样的求和
;; [f(a+dx/2) + f(a+dx+dx/2) + f(a+2dx+dx/2) + ...] * dx
;; dx 是一个极小值，无穷小量
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;; 求立方函数在 0-1 之间的定积分
(integral cube 0 1 0.00001)

;; 1.3.2 用 lambda 构造过程
;; 本质上就是匿名函数，不需要先行定义，直接将过程体传入，省去了代换过程
(define (integral2 f a b dx)
  (* (sum f (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b) dx))

;; lambda 表达式作为组合运算符
;; 因此可以将 lambda 表达式直接当成一个函数/过程 名
;; 在后面紧跟参数马上执行
(define (f x y)
  ((lambda (a b) (+ (* x (square a)) (* y b) (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

;; 实例 1
;; 结合 let 我们可以构建一个局部变量
;; (let [(<var1> <exp1>)
;;       (<var2> <exp2>)
;;       ...])
;;  <body>
;;
;; 表示为 在 body 内使用前面预先定义的临时变量
;; 用这种方式来实现类似词法作用域的约束
;; 可以将上述运算重写如下
(define (g x y)
  (let ([a (+ 1 (* x y))] [b (-1 y)])
    (+ (* x (square a)) (* y b) (* a b))))

;; 通过区间折半寻找方程的根
(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ([midpoint (average neg-point pos-point)])
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ([test-value (f midpoint)])
          (cond
            [(positive? test-value)
             (search f neg-point midpoint)]
            [(negative? test-value)
             (search f midpoint pos-point)]
            [else midpoint])))))

(define (half-interval-method f a b)
  (let ([a-value (f a)] [b-value (f b)])
    (cond
      [(and (negative? a-value) (positive? b-value))
       (search f a b)]
      [(and (negative? b-value) (positive? a-value))
       (search f b a)]
      [else
       (error "Values are not of opposite sign" a b)])))

;; 求 sinx = 0 在 2-4 之间的根 (pi)
;; (half-interval-method sin 2.0 4.0)

;; 求 x^3 - 2x - 3 = 0 在 1 和 2 之间的根
;; (half-interval-method (lambda (x) (- (cube x) (* 2 x) 3))
;;                       1.0
;;                       2.0)

;; 实例 2, 求函数不动点
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
    (let ([next (f guess)])
      ;; (display-info guess step)
      (if (close-enough? guess next)
          guess
          ;; (display "\n")
          (try next (+ 1 step)))))
  (try first-guess 1))

;; (fixed-point cos 1.0)

;; 平均阻尼函数（防止猜测值过分震荡,帮助快速收敛）
;; 高阶函数: 参数是一个函数,返回也是一个函数
(define (average-damp f)
  (lambda (x) (average x (f x))))

;; 求x^x = 1000 的一个根
;; (x->log(1000)/log(x)的一个不动点)
(define formula (lambda (x) (/ (log 1000) (log x))))

(fixed-point formula 2.0)
(fixed-point (average-damp formula) 2.0)

;; 1.3.4 过程作为返回值
;; 利用平均阻尼函数来求立方根
;; y^3 = x -> y = x / y^2
;; 转换为求 y 这个映射的不动点
;; 通过平均阻尼函数可以避免不动点猜测震荡过大
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

;; 实例: 牛顿逼近法求方程解
(define dx 0.00001)
;; 求导数的方案：过程作为返回值
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

;; 求立方函数在 5 的导数
;; ((deriv cube) 5)

;; 牛顿法求不动点
;;
;; 牛顿变换: 不动点需要使用的参数过程
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

;; 实际上就是检查牛顿变换的不动点在哪里
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

;; (sqrt 100)

;; 更高一级抽象的求转换后不动点
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(#%provide fixed-point newtons-method)
