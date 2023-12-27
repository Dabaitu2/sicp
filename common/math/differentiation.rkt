#lang sicp
(#%require "../../common/data/conventional-interface.rkt")

;; 函数求导!

;; helpers
;; 检测一个元素是否为变量而非数值
(define (variable? x)
  (symbol? x))
(define (same-variable? a b)
  (eq? a b))

;; 构造和式
;; 支持多个参数
;; 过滤出只包含数值和只包含符号的部分，前者做 accumulate 数学运算
;; 后者拼接成 list, 最终根据是否存在各个部分进行拼接成
;; opeator? num-sum? var-sum? 的形式
(define (make-sum . l)
  ;; 由于在 augend 中会传入一个形同 ('a 'b) 的 list
  ;; 而 dot notation 会将其再套一层 list, 因此需要检测这种情况，把外面那层括号剥离掉
  (let ([lst (if (null? (cdr l)) (car l) l)])
    (let ([num-list (filter number? lst)]
          [var-list (filter (lambda (x) (not (number? x))) lst)])
      (let ([num-sum (accumulate + 0 num-list)]
            [var-sum (accumulate cons '() var-list)])
        (if (= 0 num-sum)
            (cond
              [(null? var-sum) 0]
              [(= (length var-sum) 1) (car var-sum)]
              [else (cons '+ var-sum)])
            (if (null? var-sum)
                num-sum
                (cons '+ (cons num-sum var-sum))))))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product . l)
  (let ([lst (if (null? (cdr l)) (car l) l)])
    (let ([num-list (filter number? lst)]
          [var-list (filter (lambda (x) (not (number? x))) lst)])
      (let ([num-sum (accumulate * 1 num-list)]
            [var-sum (accumulate cons '() var-list)])
        ;; 没有传入过数值
        (cond ((= 1 num-sum)
               (cond
                 [(null? var-sum) 1]
                 [(= (length var-sum) 1) (car var-sum)]
                 [else (cons '* var-sum)]))
              ;; 传入了一个为 0 的数值
              ((= 0 num-sum) 0)
              ;; 既有非零数值又有符号
              (else (if (null? var-sum)
                        num-sum
                        (cons '* (cons num-sum var-sum)))))))))

;; 扩充求导规则
;; d(u^n)/dx = nu^{n-1}(du/dx)
;; 比如 d(x^2) = 2x*(dx/dx) = 2x
(define (make-exponentiation base exponent)
  (cond
    [(=number? exponent 0) 1]
    [(=number? exponent 1) base]
    [else (list '^ base exponent)]))
(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))

;; sum 和 product 支持多个参数之后，对应的 selector 也要变化
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s)
  (cadr s))
(define (augend s)
  (make-sum (cddr s)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p)
  (cadr p))
(define (multiplicand p)
  (make-product (cddr p)))

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
    [(exponentiation? exp)
     (make-product
      (exponent exp)
      (make-exponentiation
       (base exp)
       (make-sum -1 (exponent exp)))
      (deriv (base exp) var))]
    [else (error "unknown expression type -- DERIV" exp)]))

