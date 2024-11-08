#lang sicp
(#%require
 "../../../common/data/conventional-interface.rkt")
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
        (if (empty-termlist? source)
            source
            (if (=zero? (coeff (first-term source)))
                (simplify-iter (rest-terms source))
                source)))
      (simplify-iter terms)))

  (define (div-terms L1 L2)
    (if (or (empty-termlist? L1) (empty-termlist? L2))
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

  (define (reduce-terms n d)
    (let ([gcd-n-d (gcd-terms n d)])
      (list (car (div-terms n gcd-n-d))
            (car (div-terms d gcd-n-d)))))

  (define (div-coeff-list-gcd termlist)
    (let ([coeff-list (map coeff termlist)])
      (let ([gcd-coeff
             (fold-left gcd (car coeff-list) coeff-list)])
        (contents (car (div (tag termlist) gcd-coeff))))))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (div-coeff-list-gcd a)
        (gcd-terms b (pseudoremainder-terms a b))))

  (define (gcd-terms-legacy a b)
    (if (empty-termlist? b)
        a
        (gcd-terms-legacy b (pseudoremainder-terms a b))))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (pseudoremainder-terms a b)
    (let ([o1 (order (first-term a))]
          [o2 (order (first-term b))]
          [c (coeff (first-term b))])
      (let ([factor
             (expt (contents c) (contents (add 1 (sub o1 o2))))])
        (cadr (div-terms (mul-term-by-all-terms
                          (make-term 0 factor)
                          a)
                         b)))))

  (put 'pseudoremainder-terms
       '(dense dense)
       (lambda (T1 T2) (tag (pseudoremainder-terms T1 T2))))
  (put 'remainder-terms
       '(dense dense)
       (lambda (T1 T2) (tag (remainder-terms T1 T2))))

  (put 'reduce-terms
       '(dense dense)
       (lambda (T1 T2)
         (let ([terms (reduce-terms T1 T2)])
           (list (tag (car terms)) (tag (cadr terms))))))
  (put 'gcd-terms
       '(dense dense)
       (lambda (T1 T2) (tag (gcd-terms T1 T2))))
  (put 'gcd-terms-legacy
       '(dense dense)
       (lambda (T1 T2) (tag (gcd-terms-legacy T1 T2))))
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
       (lambda (T1 T2)
         (let ([terms (div-terms T1 T2)])
           (list (tag (car terms)) (tag (cadr terms))))))
  (put 'add
       '(dense dense)
       (lambda (T1 T2) (tag (add-terms T1 T2))))
  (put 'mul
       '(dense dense)
       (lambda (T1 T2) (tag (mul-terms T1 T2))))
  (put 'negate '(dense) (lambda (T) (tag (negate-terms T))))
  (put 'sub
       '(dense dense)
       (lambda (T1 T2) (tag (sub-terms T1 T2))))
  (put 'div
       '(dense dense)
       (lambda (T1 T2)
         (let ([terms (div-terms T1 T2)])
           (list (tag (car terms)) (tag (cadr terms))))))

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

  ;; melpers
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
        (the-empty-termlist)
        (let ([t (first-term termlist)])
          (adjoin-term
           (make-term (order t) (negate (coeff t)))
           (negate-terms (rest-terms termlist))))))

  (define (sub-terms L1 L2)
    (let ([terms (add-terms L1 (negate-terms L2))])
      (define (simplify-iter source)
        (if (empty-termlist? source)
            source
            (if (=zero? (coeff (first-term source)))
                (simplify-iter (rest-terms source))
                source)))
      (simplify-iter terms)))

  (define (div-terms L1 L2)
    (if (or (empty-termlist? L1) (empty-termlist? L2))
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

  (define (reduce-terms n d)
    (let ([gcd-n-d (gcd-terms n d)])
      (list (car (div-terms n gcd-n-d))
            (car (div-terms d gcd-n-d)))))

  (define (div-coeff-list-gcd termlist)
    (let ([coeff-list (map coeff termlist)])
      (let ([gcd-coeff
             (fold-left gcd (car coeff-list) coeff-list)])
        (contents (car (div (tag termlist) gcd-coeff))))))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (div-coeff-list-gcd a)
        (gcd-terms b (pseudoremainder-terms a b))))

  (define (gcd-terms-legacy a b)
    (if (empty-termlist? b)
        a
        (gcd-terms-legacy b (remainder-terms a b))))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (pseudoremainder-terms a b)
    (let ([o1 (order (first-term a))]
          [o2 (order (first-term b))]
          [c (coeff (first-term b))])
      (let ([factor
             (expt (contents c) (contents (add 1 (sub o1 o2))))])
        (cadr (div-terms (mul-term-by-all-terms
                          (make-term 0 factor)
                          a)
                         b)))))

  (put 'pseudoremainder-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (pseudoremainder-terms T1 T2))))
  (put 'remainder-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (remainder-terms T1 T2))))
  (put 'gcd-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (gcd-terms T1 T2))))
  (put 'reduce-terms
       '(sparse sparse)
       (lambda (T1 T2)
         (let ([terms (reduce-terms T1 T2)])
           (list (tag (car terms)) (tag (cadr terms))))))
  (put 'gcd-terms-legacy
       '(sparse sparse)
       (lambda (T1 T2) (tag (gcd-terms-legacy T1 T2))))
  (put 'add-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (add-terms T1 T2))))
  (put 'add
       '(sparse sparse)
       (lambda (T1 T2) (tag (add-terms T1 T2))))
  (put 'mul-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (mul-terms T1 T2))))
  (put 'mul
       '(sparse sparse)
       (lambda (T1 T2) (tag (mul-terms T1 T2))))
  (put 'negate-terms
       '(sparse)
       (lambda (T) (tag (negate-terms T))))
  (put 'negate
       '(sparse)
       (lambda (T) (tag (negate-terms T))))
  (put 'sub-terms
       '(sparse sparse)
       (lambda (T1 T2) (tag (sub-terms T1 T2))))
  (put 'sub
       '(sparse sparse)
       (lambda (T1 T2) (tag (sub-terms T1 T2))))
  (put 'div-terms
       '(sparse sparse)
       (lambda (T1 T2)
         (let ([terms (div-terms T1 T2)])
           (list (tag (car terms)) (tag (cadr terms))))))
  (put 'div
       '(sparse sparse)
       (lambda (T1 T2)
         (let ([terms (div-terms T1 T2)])
           (list (tag (car terms)) (tag (cadr terms))))))

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
  (define (reduce-terms t1 t2)
    (apply-generic 'reduce-terms t1 t2))
  (define (gcd-terms t1 t2)
    (apply-generic 'gcd-terms t1 t2))
  (define (gcd-terms-legacy t1 t2)
    (apply-generic 'gcd-terms-legacy t1 t2))
  (define (coeff term)
    (apply-generic 'coeff term))
  (define (order term)
    (apply-generic 'order term))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1)
                              (term-list p2)))
        (let ([priority-order (order-by-priority p1 p2)])
          (let ([target-var
                 (variable (car priority-order))])
            (gcd-poly (make-canonical target-var p1)
                      (make-canonical target-var p2))))))

  (define (gcd-poly-legacy p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms-legacy (term-list p1)
                                     (term-list p2)))
        (let ([priority-order (order-by-priority p1 p2)])
          (let ([target-var
                 (variable (car priority-order))])
            (gcd-poly-legacy
             (make-canonical target-var p1)
             (make-canonical target-var p2))))))

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (map (lambda (x) (make-poly (variable p1) x))
             (reduce-terms (term-list p1) (term-list p2)))
        (let ([priority-order (order-by-priority p1 p2)])
          (let ([target-var
                 (variable (car priority-order))])
            (reduce-poly (make-canonical target-var p1)
                         (make-canonical target-var p2))))))
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
           (let ([priority-order (order-by-priority p1 p2)])
             (let ([target-var
                    (variable (car priority-order))])
               (add-poly (make-canonical target-var p1)
                         (make-canonical target-var
                                         p2)))))]))

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
           (let ([priority-order (order-by-priority p1 p2)])
             (let ([target-var
                    (variable (car priority-order))])
               (mul-poly (make-canonical target-var p1)
                         (make-canonical target-var
                                         p2)))))]))

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
           (tag (make-poly (variable p1) (car results)))
           (tag (make-poly (variable p1) (cadr results)))))
        (let ([priority-order (order-by-priority p1 p2)])
          (let ([target-var
                 (variable (car priority-order))])
            (div-poly (make-canonical target-var p1)
                      (make-canonical target-var p2))))))

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

  (put 'reduce-poly
       '(polynomial polynomial)
       (lambda (p1 p2)
         (map (lambda (x) (tag x)) (reduce-poly p1 p2))))

  (put 'reduce
       '(polynomial polynomial)
       (lambda (p1 p2)
         (map (lambda (x) (tag x)) (reduce-poly p1 p2))))

  (put 'greatest-common-divisor
       '(polynomial polynomial)
       (lambda (x y) (tag (gcd-poly x y))))

  (put 'greatest-common-divisor-legacy
       '(polynomial polynomial)
       (lambda (x y) (tag (gcd-poly-legacy x y))))

  (define (order-by-priority p1 p2)
    (let ([var1 (variable p1)] [var2 (variable p2)])
      (if (string<? (symbol->string var1)
                    (symbol->string var2))
          (cons p1 p2)
          (cons p2 p1))))

  (define (make-canonical target-var poly)
    (let ([terms (term-list poly)]
          [cur-var (variable poly)])
      (if (eq? cur-var target-var)
          poly
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
                               (attach-tag
                                terms-type
                                (rest-terms terms))))
                   (let ([cof (coeff term)]
                         [ord (order term)])
                     (let ([cof-type (type-tag cof)])
                       (if (eq? cof-type 'polynomial)
                           (let ([cof-poly (contents cof)])
                             (let ([cof-poly-var
                                    (variable cof-poly)])
                               (mul-poly
                                (if (eq? cof-poly-var
                                         target-var)
                                    cof-poly
                                    (make-canonical
                                     target-var
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
                            terms-type)))))))))))

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
  (apply-generic 'term-list poly))
(define (adjoin-term term termlist)
  (apply-generic 'adjoin-term term termlist))
(define (reduce-terms n d)
  (apply-generic 'reduce-terms n d))
(define (reduce-poly p1 p2)
  (apply-generic 'reduce-poly p1 p2))
(define (make-termlist-of-type type termlist)
  (accumulate adjoin-term
              (make-empty-termlist-of-type type)
              termlist))

(define (make-canonical target-var poly)
  (attach-tag 'polynomial
              ((get 'make-canonical 'polynomial)
               target-var
               (contents poly))))

(#%provide make-polynomial
           make-term
           adjoin-term
           reduce-terms
           reduce-poly
           term-list
           the-empty-termlist
           install-polynomial-package
           make-empty-termlist-of-type
           make-canonical
           make-termlist-of-type)
