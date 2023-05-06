#lang racket
(#%require "./tag-tools.rkt")
(#%require "./env.rkt")
(#%require "./generic-tools.rkt")
(#%require "./api.rkt")

;; consts
(define (the-empty-termlist)
  '())

(define (install-term-package)
  (define (tag term)
    (attach-tag 'term term))
  (define (make-term order coeff)
    (list order coeff))
  (define (order term)
    (car term))
  (define (coeff term)
    (cadr term))

  ;; 要想能够在 apply-generic 中使用此类方法就必须加一个括号
  (put 'order '(term) order)
  (put 'coeff '(term) coeff)

  (put 'make-term
       'term
       (lambda (order coeff) (tag (make-term order coeff))))
  'done)

(define (make-term order coeff)
  ((get 'make-term 'term) order coeff))

;; 同时支持稠密多项式和系数多项式两种表示方式，参照 complex 的实现
;; 稠密多项式
(define (install-dense-termlist-package)
  ;; type defination
  (define (tag termlist)
    (attach-tag 'dense termlist))

  ;; helpers
  (define (coeff term)
    (apply-generic 'coeff term))
  (define (order term)
    (apply-generic 'order term))

  ;; selectors
  (define (first-term termlist)
    (make-term (- (length termlist)) (car termlist)))
  (define (rest-terms term-list)
    (cdr term-list))

  ;; apis
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (adjoin-term term term-list)
    (if (empty-termlist? term-list)
        term
        (list term term-list)))

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

  (define (negate-terms termlist)
    (if (empty-termlist? termlist)
        the-empty-termlist
        (let ([t (first-term termlist)])
          (adjoin-term
           (make-term (order t) (negate (coeff t)))
           (negate-terms (rest-terms termlist))))))

  (put 'adjoin-term
       '(term dense)
       (lambda (term termlist)
         (tag (adjoin-term term termlist))))
  (put 'add-terms
       '(dense dense)
       (lambda (T1 T2) (tag (add-terms T1 T2))))
  (put 'mul-terms
       '(dense dense)
       (lambda (T1 T2) (tag (mul-terms T1 T2))))
  (put 'negate-terms
       '(dense dense)
       (lambda (T1 T2) (tag (negate-terms T1 T2))))
  (put 'first-term 'dense first-term)
  (put 'rest-terms 'dense rest-terms)
  (put 'empty-termlist? 'dense empty-termlist?)

  (define (make-dense-from-sparse sparselist)
    (define (parse-iter curr-order source result)
      (if (null? source)
          result
          (let ([term (car source)]
                [rest (cdr source)]
                [next-order (+ 1 curr-order)])
            (cond
              [(< curr-order (order term))
               (parse-iter next-order
                           source
                           (cons 0 result))]
              [(> curr-order (order term))
               (error
                "dense order is over than corresponding sparse, which is not correct -- MAKE-DENSE-FROM-SPARSE"
                curr-order)]
              [else
               (parse-iter next-order
                           rest
                           (cons (coeff term) result))]))))
    (parse-iter 0 sparselist '()))

  ;; supporting raised from sparse so they can work together
  (put 'raise
       '(sparse)
       (lambda (sparselist)
         (tag (make-dense-from-sparse
               (contents sparselist)))))
  'done)

;; 稀疏多项式
(define (install-sparse-polynomial-package)

  ;; tags: type defination
  (define (tag termlist)
    (attach-tag 'sparse termlist))

  ;; helpers
  (define (coeff term)
    (apply-generic 'coeff term))
  (define (order term)
    (apply-generic 'order term))

  ;; selectors
  (define (first-term term-list)
    (car term-list))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))

  ;; apis
  (define (adjoin-term term term-list)
    (if (=zero? ((get 'coeff '(term)) term))
        term-list
        (cons term term-list)))

  ;; 如果想要支持次数乱序的多项式可以考虑使用下面的实现
  ;; (define (adjoin-term term term-list)
  ;;   (cond
  ;;     [(=zero? (coeff term))
  ;;      (if (null? term-list) the-empty-termlist term-list)]
  ;;     [(null? term-list) (list term)]
  ;;     [(= (order term) (order (first-term term-list)))
  ;;      term-list]
  ;;     [(< (order term) (order (first-term term-list)))
  ;;      (cons term term-list)]
  ;;     [else
  ;;      (cons (first-term term)
  ;;            (adjoin-term term (rest-terms term-list)))]))

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

  (define (negate-terms termlist)
    (if (empty-termlist? termlist)
        the-empty-termlist
        (let ([t (first-term termlist)])
          (adjoin-term
           (make-term (order t) (negate (coeff t)))
           (negate-terms (rest-terms termlist))))))

  (put 'adjoin-term
       '(term sparse)
       (lambda (term termlist)
         (tag (adjoin-term term termlist))))

  (put 'add-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (add-terms T1 T2))))
  (put 'mul-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (mul-terms T1 T2))))
  (put 'negate-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (negate-terms T1 T2))))
  (put 'first-term 'sparse first-term)
  (put 'rest-terms 'sparse rest-terms)
  (put 'empty-termlist? 'sparse empty-termlist?)

  (define (make-sparse-from-dense denselist)
    (define (parse-iter curr-order source result)
      (if (null? source)
          result
          (let ([coeff (car source)]
                [rest (cdr source)]
                [next-order (+ 1 curr-order)])
            (if (=zero? coeff)
                (parse-iter next-order rest result)
                (parse-iter
                 next-order
                 rest
                 (cons (make-term curr-order coeff)
                       result))))))
    (parse-iter 0 denselist '()))

  (put 'project
       '(dense)
       (lambda (x)
         (attach-tag 'sparse (make-sparse-from-dense x))))

  'done)

;; 最终的多项式库是底层的包装
(define (install-polynomial-package)

  ;; install sub module
  (install-term-package)
  (install-dense-termlist-package)
  (install-sparse-polynomial-package)

  ;; helpers
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p)
    (car p))
  (define (term-list p)
    (cdr p))

  (define (variable? x)
    (symbol? x))
  (define (same-variable? a b)
    (eq? a b))

  (define (add-terms L1 L2)
    (apply-generic 'add-terms L1 L2))
  (define (mul-terms L1 L2)
    (apply-generic 'mul-terms L1 L2))
  (define (negate-terms L)
    (apply-generic 'negate-terms L))
  (define (empty-termlist? L)
    (apply-generic 'empty-termlist? L))
  (define (first-term L)
    (apply-generic 'first-term L))
  (define (rest-terms L)
    (apply-generic 'rest-terms L))
  (define (coeff term)
    (apply-generic 'coeff term))

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

  (put 'negate
       '(polynomial)
       (lambda (poly)
         (make-polynomial (variable poly)
                          (negate-terms (term-list poly)))))
  (put 'make
       'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) zero-poly?)
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (adjoin-term term termlist)
  (apply-generic 'adjoin-term term termlist))

(#%provide make-polynomial
           make-term
           adjoin-term
           the-empty-termlist
           install-polynomial-package)
