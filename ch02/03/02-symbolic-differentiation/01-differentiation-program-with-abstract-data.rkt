#lang racket

;; 符号求导
;; 实现一个过程，传入一个 代数表达式 和一个变量，返回这个表达式相对于该变量的导数
;; 例如 ax^2 + bx + c 返回 2ax + b
;; 这里会使用和 2.1.1 一样的方案，使用抽象屏障
;; 首先定义一个求导算法，令他在一些抽象对象上操作，例如 add, multiply or variable
;; 然后再去考虑这些具体表示的问题

;; 我们在这里考虑的是简单版的求导程序：只处理建立在两个参数间使用 + 和 * 的表达式这一前提上
;; 满足这些情况
;; dc/dx = 0 (c = constant, dx is 极小值)
;; dx/dx = 1 (前面的 d 是求 x 这个函数的微分，后面的是极小值)
;; d(u+v)/dx = du/dx + dv/dx (满足分配率)
;; d(uv)/dx = u(dv/dx) + v(du/dx)

;; 单一变量相关的判断函数
;; (variable? e)
;; (same-variable? v1 v2)

;; 加法相关的 constructor 和 selectors
;; (sum? e)
;; (addend e) ;; 被加数
;; (augend e) ;; 加数
;; (make-sum a1 a2) ;; 构造一个和式

;; 乘法相关的 constructor 和 selectors
;; (product? e)
;; (multiplier e) ;; 被乘数
;; (multiplicand e) ;; 乘数
;; (make-product m1 m2) ;; 构造一个乘式

;; 假设我们已经有了这些表达式，那么我们就可以这样构造出我们需要的求导算法
;; 求 exp 关于 var 的导数
(define (deriv exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp) (if (same-variable? exp var) 1 0)]
    [(sum? exp)
     (make-sum (deriv (addend exp) var)
               (deriv (augend exp) var))]
    [(product? exp)
     (make-sum (make-product (multiplier exp)
                             (deriv (multiplicand exp) var))
               (make-product (multiplicand exp)
                             (deriv (multiplier exp) var)))]
    [else (error "unknown expression type -- DERIV" exp)]))

;; 接下来实现表达式本身，我们遵循 scheme 规则，采用前缀式
;; 变量就是符号，直接用系统提供的 symbol? 和 eq?
(define (variable? x)
  (symbol? x))
(define (same-variable? a b)
  (eq? a b))

;; 这里我们没有用 ' 代表 a1, a2, 这是因为他们作为数字依然是被承认的
(define (make-sum-1 a1 a2) (list '+ a1 a2))
(define (make-product-1 m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))


(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;; 如果代码仅仅到这里为止，我们可以求到正确的结果，但是这个结果没有化简
;; (deriv '(+ x 3) 'x)
;; (deriv '(* x y) 'x)
;; (deriv '(* (* x y) (+ x 3)) 'x)


;; 因此我们将 make-sum 和 make-product 进行改造，使得它能够直接化简
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; 化简后的结果就好很多了
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
