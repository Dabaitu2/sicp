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
  (put 'equ?
       '(term term)
       (lambda (x y)
         (and (= (coeff x) (coeff y))
              (= (order x) (order y)))))

  (put 'make-term
       'term
       (lambda (order coeff) (tag (make-term order coeff))))

  (put 'raise '(complex) (lambda (x) (tag (make-term 0 x))))

  'done)

(define (make-term order coeff)
  ((get 'make-term 'term) order coeff))

;; 同时支持稠密多项式和系数多项式两种表示方式，参照 complex 的实现
;; 稠密多项式
;; 内部使用的 api 由于通常会被 apply-generic 使用, 因此会把 tag 剥离掉
;; 因此需要注意不可以和其他使用 apply-generic 的元素混用,
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
    (make-term (- (length termlist) 1) (car termlist)))
  (define (rest-terms term-list)
    (cdr term-list))

  ;; apis
  (define (empty-termlist? term-list)
    (null? term-list))

  (define (adjoin-term term term-list)
    (let ([term-order (order term)]
          [max-list-order (sub (length term-list) 1)])
      (cond
        [(= term-order max-list-order)
         (cons (add (coeff term)
                    (coeff (first-term term-list)))
               (rest-terms term-list))]
        [(> term-order max-list-order)
         (adjoin-term term (cons 0 term-list))]
        [else (adjoin-term term (rest-terms term-list))])))

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
         (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ([t2 (first-term L)])
          (adjoin-term
           (make-term (add (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (negate-terms termlist)
    (if (empty-termlist? termlist)
        (the-empty-termlist)
        (let ([t (first-term termlist)])
          (adjoin-term
           (make-term (order t) (negate (coeff t)))
           (negate-terms (rest-terms termlist))))))

  (define (sub-terms L1 L2)
    (let ([terms (add-terms L1 (negate-terms L2))])
      (define (simplify-iter source)
        (if (=zero? (coeff (first-term source)))
            (simplify-iter (rest-terms source))
            source))
      (simplify-iter terms)))

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
                        (sub-terms L1
                                   (mul-term-by-all-terms
                                    (make-term new-o new-c)
                                    L2))
                        L2)])
                  ;; 组合形成完整结果
                  (list (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cadr rest-of-result))))))))

  (define (_equ? x y)
    (cond
      [(and (null? x) (null? y)) #t]
      [(= (length x) (length y))
       (if (equ? (car x) (car y))
           (_equ? (cdr x) (cdr y))
           #f)]
      [else #f]))

  (put
   'adjoin-term
   '(term dense)
   (lambda (_term term-list)
     (let ([term-order ((get 'order '(term)) _term)]
           [term-coeff ((get 'coeff '(term)) _term)]
           [max-list-order (sub (length term-list) 1)])
       (let ([term (make-term term-order term-coeff)])
         (tag (cond
                [(= term-order max-list-order)
                 (cons ((add term-coeff
                             (coeff (first-term term-list)))
                        (rest-terms term-list)))]
                [(> term-order max-list-order)
                 (adjoin-term term (cons 0 term-list))]
                [else
                 (adjoin-term term
                              (rest-terms term-list))]))))))

  (put 'add-terms
       '(dense dense)
       (lambda (T1 T2) (tag (add-terms T1 T2))))
  (put 'mul-terms
       '(dense dense)
       (lambda (T1 T2) (tag (mul-terms T1 T2))))
  (put 'negate-terms
       '(dense)
       (lambda (T) (tag (negate-terms T))))
  (put 'sub-terms
       '(dense dense)
       (lambda (T1 T2) (tag (sub-terms T1 T2))))
  (put 'div-terms
       '(dense dense)
       (lambda (T1 T2) (tag (div-terms T1 T2))))

  (put 'first-term 'dense first-term)
  (put 'rest-terms 'dense rest-terms)
  (put 'empty-termlist? 'dense empty-termlist?)
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense) rest-terms)
  (put 'empty-termlist? '(dense) empty-termlist?)
  (put 'equ? '(dense dense) _equ?)

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
    (parse-iter 0 (reverse sparselist) '()))

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
  ;; (define (adjoin-term term term-list)
  ;;   (if (=zero? (coeff term))
  ;;       term-list
  ;;       (cons term term-list)))

  ;; 如果想要支持次数乱序的多项式可以考虑使用下面的实现
  (define (adjoin-term term term-list)
    (cond
      [(=zero? (coeff term)) term-list]
      [(empty-termlist? term-list) (cons term term-list)]
      [(= (order term) (order (first-term term-list)))
       (cons (make-term (order term)
                        (add (coeff term)
                             (coeff (first-term
                                     term-list))))
             (rest-terms term-list))]
      [(> (order term) (order (first-term term-list)))
       (cons term term-list)]
      [else
       (cons (first-term term-list)
             (adjoin-term term (rest-terms term-list)))]))

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
         (mul-terms (rest-terms L1) L2))))

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

  (define (sub-terms L1 L2)
    (let ([terms (add-terms L1 (negate-terms L2))])
      (define (simplify-iter source)
        (if (=zero? (coeff (first-term source)))
            (simplify-iter (rest-terms source))
            source))
      (simplify-iter terms)))

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
                        (sub-terms L1
                                   (mul-term-by-all-terms
                                    (make-term new-o new-c)
                                    L2))
                        L2)])
                  ;; 组合形成完整结果
                  (list (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cadr rest-of-result))))))))

  (define (_equ? x y)
    (cond
      [(and (null? x) (null? y)) #t]
      [(= (length x) (length y))
       (if (equ? (car x) (car y))
           (_equ? (cdr x) (cdr y))
           #f)]
      [else #f]))

  (put
   'adjoin-term
   '(term sparse)
   (lambda (_term term-list)
     (let ([term-order ((get 'order '(term)) _term)]
           [term-coeff ((get 'coeff '(term)) _term)])
       (let ([term (make-term term-order term-coeff)])
         (tag
          (cond
            [(=zero? term-coeff) term-list]
            [(empty-termlist? term-list)
             (cons (make-term term-order term-coeff)
                   term-list)]
            [else
             (let ([next-term (first-term term-list)]
                   [rest-term-list (rest-terms term-list)])
               (cond
                 [(= term-order (order next-term))
                  (cons (make-term term-order
                                   (add term-coeff
                                        (coeff next-term)))
                        rest-term-list)]
                 [(> term-order (order next-term))
                  (cons term term-list)]
                 [else
                  (cons next-term
                        (adjoin-term
                         term
                         rest-term-list))]))]))))))

  (put 'add-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (add-terms T1 T2))))
  (put 'mul-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (mul-terms T1 T2))))
  (put 'negate-terms
       '(sparse)
       (lambda (T) (tag (negate-terms T))))
  (put 'sub-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (sub-terms T1 T2))))
  (put 'div-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (div-terms T1 T2))))

  (put 'first-term 'sparse first-term)
  (put 'rest-terms 'sparse rest-terms)
  (put 'empty-termlist? 'sparse empty-termlist?)

  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) rest-terms)
  (put 'empty-termlist? '(sparse) empty-termlist?)
  (put 'equ? '(sparse sparse) _equ?)

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
    (parse-iter 0 (reverse denselist) '()))

  (put 'raise
       '(term)
       (lambda (x)
         (tag (adjoin-term x (the-empty-termlist)))))

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
  (define (div-terms L1 L2)
    (apply-generic 'div-terms L1 L2))
  (define (empty-termlist? L)
    (apply-generic 'empty-termlist? L))
  (define (first-term L)
    (apply-generic 'first-term L))
  (define (rest-terms L)
    (apply-generic 'rest-terms L))
  (define (coeff term)
    (apply-generic 'coeff term))
  (define (order term)
    (apply-generic 'order term))

  ;; 多项式相加的本质就是每一个对应 term 相加 的组合
  ;; 对应的 term 应该具有对应的 乘方数
  ;; new logic: 对于持有特殊未知数 variable 的 poly, 一定是 raise 上来的 datum,
  ;; 可以支持其直接相加/乘 (进而支持除法和减法)，并转换为被加 poly 的 variable
  (define (add-poly p1 p2)
    (cond
      [(eq? '$ (variable p1))
       (make-poly (variable p2)
                  (add-terms (term-list p1)
                             (term-list p2)))]
      [(eq? '$ (variable p2))
       (make-poly (variable p1)
                  (add-terms (term-list p1)
                             (term-list p2)))]
      [else
       (if (same-variable? (variable p1) (variable p2))
           (make-poly (variable p1)
                      (add-terms (term-list p1)
                                 (term-list p2)))
           (error "Polys not in same var -- ADD-POLY"
                  (list p1 p2)))]))

  (define (mul-poly p1 p2)
    (cond
      [(eq? '$ (variable p1))
       (make-poly (variable p2)
                  (mul-terms (term-list p1)
                             (term-list p2)))]
      [(eq? '$ (variable p2))
       (make-poly (variable p1)
                  (mul-terms (term-list p1)
                             (term-list p2)))]
      [else
       (if (same-variable? (variable p1) (variable p2))
           (make-poly (variable p1)
                      (mul-terms (term-list p1)
                                 (term-list p2)))
           (error "Polys not in same var -- MUL-POLY"
                  (list p1 p2)))]))

  (define (zero-poly? poly)
    (define (zero-terms? termlist)
      (or (empty-termlist? termlist)
          (and (=zero? (coeff (first-term termlist)))
               (zero-terms? (rest-terms termlist)))))
    (zero-terms? (term-list poly)))

  (define (sub-poly p1 p2)
    (add-poly p1
              (contents ((get 'negate '(polynomial)) p2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable? p1) (variable? p2))
        (let ([results (div-terms (term-list p1)
                                  (term-list p2))])
          (list
           (tag (make-poly (variable p1) (cadr results)))
           (tag (make-poly (variable p1) (caddr results)))))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (_equ? x y)
    (and (eq? (variable x) (variable y))
         (equ? (term-list x) (term-list y))))

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

  (put 'div
       '(polynomial polynomial)
       (lambda (p1 p2) (div-poly p1 p2)))

  (put 'negate
       '(polynomial)
       (lambda (poly)
         (make-polynomial (variable poly)
                          (negate-terms (term-list poly)))))

  (put 'make
       'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) zero-poly?)
  (put 'equ? '(polynomial polynomial) _equ?)
  (put 'term-list '(polynomial) term-list)
  (put 'raise '(dense) (lambda (x) (tag (make-poly '$ x))))

  (define (order-by-priority p1 p2)
    (let ([var1 (variable p1)] [var2 (variable p2)])
      (if (string<? (symbol->string var1)
                    (symbol->string var2))
          (cons p1 p2)
          (cons p2 p1))))

  (define (make-canonical target-var poly)
    (let ([terms (term-list poly)]
          [cur-var (variable poly)])
      (let ([terms-type (type-tag terms)])
        (if (empty-termlist? terms)
            (make-poly target-var
                       (make-empty-termlist-of-type
                        terms-type))
            (let ([term (first-term terms)])
              (add-poly
               (make-canonical
                target-var
                (make-poly cur-var
                           (attach-tag terms-type
                                       (rest-terms terms))))
               (let ([cof (coeff term)] [ord (order term)])
                 (let ([cof-type (type-tag cof)])
                   (if (eq? cof-type 'polynomial)
                       (let ([cof-poly (contents cof)])
                         (let ([cof-poly-var
                                (variable cof-poly)])
                           (mul-poly
                            (if (eq? cof-poly-var
                                     target-var)
                                cof-poly
                                (make-canonical target-var
                                                cof-poly))
                            (make-poly-from-single-term
                             target-var
                             cur-var
                             ord
                             1
                             terms-type))))
                       ;; if coeff is not polynomial, which means this term has no relationship
                       ;; with target-var at all, we can just use whole terms as the coeff of the new poly with 0 order
                       (make-poly-from-single-term
                        target-var
                        cur-var
                        ord
                        cof
                        terms-type))))))))))

  (define (make-poly-from-single-term target-var
                                      cur-var
                                      ord
                                      cof
                                      terms-type)
    (make-poly
     target-var
     (adjoin-term
      (make-term 0
                 (attach-tag
                  'polynomial
                  (make-poly cur-var
                             (adjoin-term
                              (make-term ord cof)
                              (make-empty-termlist-of-type
                               terms-type)))))
      (make-empty-termlist-of-type terms-type))))

  (put 'make-canonical 'polynomial make-canonical)
  'done)

(define (make-empty-termlist-of-type type)
  (attach-tag type (the-empty-termlist)))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (term-list poly)
  ((get '(polynomial)) poly))
(define (adjoin-term term termlist)
  (apply-generic 'adjoin-term term termlist))

(define (make-canonical target-var poly)
  (attach-tag 'polynomial
              ((get 'make-canonical 'polynomial)
               target-var
               (contents poly))))

(#%provide make-polynomial
           make-term
           adjoin-term
           term-list
           the-empty-termlist
           install-polynomial-package
           make-empty-termlist-of-type
           make-canonical)
