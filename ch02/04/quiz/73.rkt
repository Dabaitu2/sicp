#lang racket
(#%require "../03-data-directed-and-additivity/tagged.rkt")
(#%require "../../../common/data/conventional-interface.rkt")

;; 检测一个元素是否为变量而非数值
(define (variable? x)
  (symbol? x))
(define (same-variable? a b)
  (eq? a b))


(define (install-add-package)
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (addend s)
    (cadr s))
  (define (augend s)
    (make-sum (cddr s)))
  (define (tag x)
    (attach-tag '+ x))
  (define (make-sum . l)
    (let ([lst (if (null? (cdr l)) (car l) l)])
      (let ([num-list (filter number? lst)]
            [var-list
             (filter (lambda (x) (not (number? x))) lst)])
        (let ([num-sum (accumulate + 0 num-list)]
              [var-sum (accumulate cons '() var-list)])
          (if (= 0 num-sum)
              (cond
                [(null? var-sum) 0]
                [(= (length var-sum) 1) (car var-sum)]
                [else (tag var-sum)])
              (if (null? var-sum)
                  num-sum
                  (tag (cons num-sum var-sum))))))))
  (put 'make-sum '+ make-sum)
  (put 'deriv '+ (lambda (exp var)
                   (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))))
  'done)
(define (make-sum . l)
  (apply (get 'make-sum '+) l))

(define (install-product-package)
  ;; 构造乘积表达式
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p)
    (cadr p))
  (define (multiplicand p)
    (make-product (cddr p)))
  (define (tag x)
    (attach-tag '* x))
  (define (make-product . l)
    (let ([lst (if (null? (cdr l)) (car l) l)])
      (let ([num-list (filter number? lst)]
            [var-list
             (filter (lambda (x) (not (number? x))) lst)])
        (let ([num-sum (accumulate * 1 num-list)]
              [var-sum (accumulate cons '() var-list)])
          ;; 没有传入过数值
          (cond
            [(= 1 num-sum)
             (cond
               [(null? var-sum) 1]
               [(= (length var-sum) 1) (car var-sum)]
               [else (tag var-sum)])]
            ;; 传入了一个为 0 的数值
            [(= 0 num-sum) 0]
            ;; 既有非零数值又有符号
            [else
             (if (null? var-sum)
                 num-sum
                 (tag (cons num-sum var-sum)))])))))
  (put 'make-product '* make-product)
  ;; 本质上我们的 make-product 可以进一步扩充为支持任意参数
  ;; 但是由于这里 deriv 的通用型定义设计如此 (只支持两个参数)
  ;; 我们就暂时只搞两个参数，后面有机会再扩充
  (put 'deriv '* (lambda (exp var)
                   (make-sum (make-product (multiplier exp)
                                           (deriv (multiplicand exp) var))
                             (make-product (multiplicand exp)
                                           (deriv (multiplier exp) var)))))
  'done)
(define (make-product . l)
  (apply (get 'make-product '*) l))


;; 构造指数表达式
(define (install-exponentiation-package)
  (define (base x)
    (cadr x))
  (define (exponent x)
    (caddr x))
  (define (exponentiation? x)
    (and (pair? x) (eq? (car x) '^)))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-exponentiation base exponent)
    (cond
      [(=number? exponent 0) 1]
      [(=number? exponent 1) base]
      [else (list '^ base exponent)]))
  (put 'make-exponentiation '^ make-exponentiation)
  (put 'deriv '^  (lambda (exp var)
                    (make-product (exponent exp)
                                  (make-exponentiation
                                   (base exp)
                                   (make-sum -1 (exponent exp)))
                                  (deriv (base exp) var))))
  'done)

;; 将 make-product / make-sum / make-exponentiation 等操作通用化
;; 之所以 variable 和 number 无法也通过数据导向设计进行 deriv 分派, 是因为
;; 1. variable 和 number 不是代数运算类型, 不符合数据导向定义
;; 2. 它们都只有一个参数，和 deriv 所需要的参数不符, 可以说是不符合泛型定义
(define (deriv exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp) (if (same-variable? exp var) 1 0)]
    [else
     ((get 'deriv (operator exp)) (operands exp) var)]))

(define (opeator exp)
  (car exp))
(define (operand exp)
  (cdr exp))
