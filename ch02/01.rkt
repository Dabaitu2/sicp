#lang sicp

;; numer 求分子
;; denom 求分母
;; 有理数的四则运算
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y)) (* (numer x) (denom y))))
;; cons 创建一组序对用于粘合两个数据
(define x (cons 1 2))

;; cons = Construct 表示构造
;; car = Contents of Address part of Register 存储地址中的“地址”
;; cdr = Contents of Decrement part of Register 存储地址中的“减量”
;; 但在这里他们就是为了取出 Cons 的前半部分和后半部分的
(car x)
(cdr x)

;; 通过序对来模拟有理数的整数部分和小数部分
;; 获取序对分子分母的最大公约数，然后将分子分母化简为真分数
(define (make-rat n d)
  (let ([g ((if (< d 0) - +) (gcd n d))])
    (cons (/ n g) (/ d g))))

(define (numer x)
  (car x))
(define (denom x)
  (cdr x))

;; 打印出有理数为小数表示
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat -1 2))
(define one-third (make-rat 1 3))
(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))

;; 2.1.3 数据意味着什么？
;; 一般而言我们将数据定义为
;; 1 & 2 一组适当的 Selector 和 Constructor
;; 3 以及使这些过程成为一套合法表示所必须满足的一组特定条件
;; 下面展示了如何将 Pair 也看待成数据的

;; 这里出现了闭包！我们返回了一个过程，而这个过程中存储了数据
;; 怪不得说 js 从 lisp 中吸取了大量经验
;; 这种将过程作为对象传递的方式被称为 Message passing 数据传递
(define (cons x y)
  (define (dispatch m)
    (cond
      [(= m 0) x]
      [(= m 1) y]
      [else (error "Argument not 0 or 1 -- CONS" m)]))
  (dispatch))

(define (car z)
  (z 0))
(define (cdr z)
  (z 1))

;; 2.1.4 实例: 区间算数
;; 抽象的几个要点
;; 1. 像 TDD 一样的先设计用户层的数据操作，wishful thinking
;; 2. 逐层设计 abstract barrier 隔离操作和实现
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; 由于区间可能存在负数，因此我们需要考虑所有情况
(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4))))

;; x / y => x * 1 / y
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (make-interval a b)
  (cons a b))

(define (lower-bound c)
  (min (car c) (cdr c)))

(define (upper-bound c)
  (max (car c) (cdr c)))

(define i (make-interval 2 7))
(upper-bound i)
(lower-bound i)

(define j (make-interval 8 3))
(upper-bound j)
(lower-bound j)


;; 使用另一种形式来描述区间
;; 指定中点和误差值，如 3.5±10
;; 它的底层仍然是区间算数
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; 通过百分比来定义区间
(define (make-center-percent c p)
  (let ([w (* (/ p 100) c)])
    (make-center-width c w)))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

