#lang sicp

;; 多项式的除法：
;; 一个单变元多项式可以除以另一个多项式，产出一个商式和一个余式
;; quotient & remainder
;; e.g: x^5 - 1 / x^2 - 1 = X^3 + x 余式 = x - 1
;;
;; introduce long division:
;;
;; x^5 - 1 / x^2 - 1
;;
;; quotient first item: x^3
;;
;; x^3 * (x^2 - 1) = x^5 - x^3
;;
;; new dividend:
;; x^5 - 1 - (x^5 - x^3) = x^3 - 1
;;
;; x^3 - 1 / x^2 - 1
;;
;; quotient second item x^3 / x^2 = x
;;
;; x * (x^2 - 1) = x^3 - x
;;
;; new dividend:
;; (x^3 - 1) - (x^3 - x) = x - 1
;;
;; whose order is lower than divisor, stop recursive procedure
;;     remainder is x - 1
;; and quotient is x^3 + x

;; helpers
(define (variable? a)
  (symbol? a))
(define (variable p)
  (car p))
(define (term-list p)
  (cdr p))
(define (same-variable? a b)
  (eq? a b))
(define (the-empty-termlist)
  '())
(define (make-poly variable term-list)
  (cons variable term-list))
(define (first-term term-list)
  (car term-list))
(define (rest-terms term-list)
  (cdr term-list))
(define (empty-termlist? term-list)
  (null? term-list))

(define (make-term order coeff)
  (list order coeff))
(define (order term)
  (car term))
(define (coeff term)
  (cadr term))

(define (div a b)
  (display "placeholder"))

(define (sub-terms a b)
  (display "placeholder"))

(define (mul-term-by-all-terms a b)
  (display "placeholder"))

(define (adjoin-term a b)
  (display "placeholder"))


;; answer
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ([t1 (first-term L1)] [t2 (first-term L2)])
        (if (> (order t2) (order t1))
            ;; if order does not meet the condition
            ;; we will take the dividend as the final remainder
            (list (the-empty-termlist) L1)
            ;; c coeff
            ;; o order
            (let ([new-c (div (coeff t1) (coeff t2))]
                  [new-o (- (order t1) (order t2))])
              (let ([rest-of-result
                     ;; 递归计算部分
                     (div-terms
                      (sub-terms
                       L1
                       (mul-term-by-all-terms
                        (make-term new-o new-c)
                        L2))
                      L2)])
                ;; 组合形成完整结果
                (list (adjoin-term (make-term new-o new-c)
                                   (car rest-of-result))
                      (cadr rest-of-result))))))))

(define (div-poly p1 p2)
  (if (same-variable? (variable? p1) (variable? p2))
      (let ((results
             (div-terms (term-list p1)
                        (term-list p2)))
            )
        (list (make-poly (variable p1) (car results))
              (cadr results)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2)))
  )
