#lang racket

;; helpers
(define (variable? x)
  (symbol? x))
(define (same-variable? a b)
  (eq? a b))

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

;; 扩充求导规则
;; d(u^n)/dx = nu^{n-1}(du/dx)
;; 比如 d(x^2) = 2x*(dx/dx) = 2x

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '^ base exponent))
        ))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))


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
      (make-product
       (make-exponentiation
        (base exp)
        (make-sum -1 (exponent exp)))
       (deriv (base exp) var)))]
    [else (error "unknown expression type -- DERIV" exp)]))

(deriv '(^ x 3) 'x)
