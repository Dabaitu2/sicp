#lang sicp

;; 1.1.1 expression: 前缀表达式
(* 1 2)

;; lisp lsp 通常支持换行时自动对齐
(+ (* 3 (+ 4 5) (- 10 7)) (/ 10 2))

;; 1.1.2
;; define variable
(define size 2)

;; 1.4 compose procedure
;; (define (<name> <formal parameters) (* x x))
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5) ;; -> 136

;; 1.1.5 代换模型
;; (f 5) -> 寻找 f 对应的 body 即 (sum-of-squares (+ a 1) (* a 2))
;;          将实参替换形参即 (sum-of-squares (+ 5 1) (* 5 2))
;;          计算基本表达式   (sum-of-squares 6 10)
;;          寻找 sum-of-squares 对应的 body 即 (+ (square x) (square y))
;;          将实参替换形参即 (+ (square 6) (square 10))
;;          寻找 square 对应的 body 即 (* x x)
;;          将实参替换形参即 (+ (* 6 6) (* 10 10))
;;          计算基本表达式，规约出最终结果 136
;;
;; 代换模型不是解释器的实际工作方式的具体描述
;; 实际一般采用形式参数的局部环境去产生代换效I
;; 规范序和应用序
;; 上面代换模型中是先求基本数值再将计算后的值代换形式参数，这称为 "应用序"
;; 而还有一种方案是将所有的过程代换全部展开后再计算基本数值完成规约，这称为 “规范序"
;; lisp 使用的是应用序，因为可以避免一些重复计算，比如 (square (+ x y))
;; 采用规范序会变成 (* (+ x y) (+ x y)) 而应用序会只算一次 (+ x )
;; TODO 当然规范序还是有可用之处的

;; 1.1.6 cond expression and predicates
;; cond 有点像是 match pattern
;; cond 中 e 可以是一个表达式序列, 满足 prediccate 会依次求值, 将最后一个表达式结果返回
;; 最后一个会被作为 cond 的结果返回
;;
;; p -> predicate     断言
;; e -> expression    假设为真执行的表达式
;; (<p> <e>)          称为 clause 分句
;;
;; (cond (<p1> <e1>)
;;       (<p2> <e2>)
;;       ...)
(define (abs x)
  (cond
    [(> x 0) x]
    [(= x 0) 0]
    [(< x 0) (- x)]))

;; 也支持用 else 来指定类似 defualt 的语法
(define (abs2 x)
  (cond
    [(< x 0) (- x)]
    [else x]))

;; if 是条件表达式的受限形式，只用于分析情况中只有两个情况的需要
;; expression 不支持表达式序列，只能是单个表达式
(define (abs3 x)
  (if (< x 0) (- x) x))

;; and 断言：和 && 表现一致
;; or 断言: 和 || 表现一致
;; nor 断言: 和 ! 表现一致
(define (clamp x)
  (and (< x 10) (> x 5)))

(define (>= x y)
  (or (> x y) (= x y)))

;; 或者 (define (>= x y)
;;        (not (< x y)))

;; 1.1.7 牛顿法求平方根
;; 我们通常在定义一个求平方根的函数时，会这样表述
;; √x = 那样的 y 使得 y >= 0 且 y^2 = x
;; 但这只是 说明描述，而不是 “行动描述" 意味着我们无法
;; 将说明描述写成一个实际可用执行的程序，因为我们永远只是在重复原来的问题
;; 因此，我们需要行动化的描述，通常可以用牛顿法逼近
;; 如果我们对 x 的平方根有了一个猜测 y, 那么 (avg y x/y) 一定是一个更准确的猜测

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess x (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; 1.1.8 作为黑盒抽象的过程
(define (local-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(local-sqrt 9.0)
