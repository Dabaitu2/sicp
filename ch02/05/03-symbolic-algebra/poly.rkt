#lang racket

(#%require "./tag-tools.rkt")
(#%require "./env.rkt")
(#%require "./api.rkt")

;; 构造 term, 为 次数-系数 的序列，且只保存系数不为 0 的项，
;; 这样做是为了规避稀疏多项式 (大部分次数为 0, 比如 x^100 + x + 1)
;; 可能产生大量系数为 0 的无用序列 从而造成浪费
(define (make-term order coeff)
  (list order coeff))
(define (order term)
  (car term))
(define (coeff term)
  (cadr term))

;; (define (adjoin-term term term-list)
;;   (cond
;;     [(=zero? (coeff term)) term-list]
;;     [(= (order term) (order (first-term term-list)))
;;      term-list]
;;     [(< (order term) (order (first-term term-list)))
;;      (cons term term-list)]
;;     [else
;;      (cons (first-term term-list)
;;            (adjoin-term term (rest-terms term-list)))]))

;; 将 term 和 termlist 进行拼接
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)
      ))

;; adjoin-term 不是用来将 term 和 term-list 相加的，
;; 更可以理解为一个内部方法，用来拼接而已
;; 这里的写法比书中的更完整一点, 这样可以对构建 termlist 时的顺序不加预设

;; 定义一系列的 constructor 和 selector
;; 定义空的 termlist 常量
(define (the-empty-termlist)
  '())

(define (first-term term-list)
  (car term-list))
(define (rest-terms term-list)
  (cdr term-list))
(define (empty-termlist? term-list)
  (null? term-list))

(define (install-polynomial-package)
  ;; constructor, make polynomial
  ;; internal apis, not tag!
  (define (make-poly variable term-list)
    (cons variable term-list))

  ;; seletors, get variable （like x) ot term-list () from poly
  (define (variable p)
    (car p))
  (define (term-list p)
    (cdr p))

  (define (variable? x)
    (symbol? x))
  (define (same-variable? a b)
    (eq? a b))

  (define (add-terms L1 L2)
    (cond
      [(empty-termlist? L1) L2]
      [(empty-termlist? L2) L1]
      [else
       (let ([t1 (first-term L1)] [t2 (first-term L2)])
         (cond
           [(> (order t1) (order t2))
            (adjoin-term t1 (add-terms (rest-terms L1) L2))]
           [(< (order t1) (order t2))
            (adjoin-term t2 (add-terms L1 (rest-terms L2)))]
           [else
            (adjoin-term
             (make-term (order t1)
                        (add (coeff t1) (coeff t2)))
             (add-terms (rest-terms L1)
                        (rest-terms L2)))]))]))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms
         (mul-term-by-all-terms (first-term L1) L2)
         (mul-terms (rest-terms) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ([t2 (first-term L)])
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  ;; 多项式相加的本质就是每一个对应 term 相加 的组合
  ;; 对应的 term 应该具有对应的 乘方数
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (zero-poly? poly)
    (define (zero-terms? termlist)
      (or (empty-termlist? termlist)
          (and (=zero? (coeff (first-term termlist)))
               (zero-terms? (rest-terms termlist)))))
    (zero-terms? (term-list poly)))

  (define (negate-terms termlist)
    (if (empty-termlist? termlist)
        the-empty-termlist
        (let ([t (first-term termlist)])
          (adjoin-term
           (make-term (order t) (negate (coeff t)))
           (negate-terms (rest-terms termlist))))))

  (define (negate poly)
    (make-polynomial (variable poly) (term-list poly)))

  (define (sub-poly p1 p2)
    (add-poly p1 (negate p2)))

  (define (tag p)
    (attach-tag 'polynomial p))

  (put 'add
       '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'mul
       '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'sub
       '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))

  (put 'negate '(polynomial) negate)

  (put 'make
       'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  (put '=zero? '(polynomial) zero-poly?)

  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(#%provide make-polynomial
           make-term
           adjoin-term
           the-empty-termlist
           install-polynomial-package)
