#lang sicp

(#%require "../03-symbolic-algebra/poly.rkt")
(#%require "../03-symbolic-algebra/tag-tools.rkt")

;; TL,DR;
;; 1. 不同 variable 的 polynomial 的运算涉及到统一 variable 的操作, 因此需要 make-canonical 使得不同 varibale 的 poly 转为相同
;; 2. 转为相同后，可能存在 coeff 同时包含 poly 和 datam 的状况，因此要支持这两者之间进行运算。可以通过将 datum raise 到具有特殊 variable 的 poly 来实现
;;    这样, 只要 poly 相关的内部运算一旦遇到这个 特殊 varaible 就会知道如何处理
;; 3. 遇到不同 variable  的 poly 时, 我们需要确定将哪个 poly 转成 canonical form, 因此需要对变量进行一个预先排序 (priority-order)
;;
;;
;; ((3X^2 + 4X)z^2 + z)y^2 => variable y, term (make-term 2 (make-poly 'x (term-list (2 3) (1 4))))
;;
;; *It refer to this article's solution, a lot of thanks!*
;;
;; *** https://www.inchmeal.io/sicp/ch-2/ex-2.92.html ***
;;
;; why do we need `canonical form`?
;; to make poly with different variables turn to the polys with the same varibles,
;; then we can using previous mul-logic to multiply them
;;
;; the way to make polynomial canonical is recursive
;; the main idea to make terms canonical is
;; 1. split it into list of single term
;; 2. transform thems one by one, for each term, we have a coeff and order
;;    a. check if the coeff was a polynomial
;;    b. if not (which means it's just a datum), raise it to polynomial that we want it to be (with specified variable like 'x'), if it was, proceeding to next step
;;    c. now it's a polynomial, then we make y^2 as a polynomial of x too.
;;       as (make-poly x (make-termlist (make-term 0 (polynomial y (2 1)))))
;;    d. now both part of original term is polynomial of `x`, we can use mul-poly too calculate them!
;;    ps: to achieve that, we need to support generic function `mul` and `add` for calcuating termlist and other datum, and we need to
;;        maintain the raise logic to let datum can be rasied to term
;;
;; 3. add them, the add-poly logic will automatic handle aggregating of term with same order

;; this function will be put inside `install-polynomial-package`
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

;; after making polynomial with lower priority variable into canonical form
;; we can use original methods to handle the computation between them

(define (order-by-priority p1 p2)
  (let ([var1 (variable p1)] [var2 (variable p2)])
    (if (string<? (symbol->string var1)
                  (symbol->string var2))
        (cons p1 p2)
        (cons p2 p1))))


;; besides that, we need to keep all the type layer work
(put 'raise
     '(real)
     (lambda (x)
       (tag (make-from-real-imag (contents x) 0))))
(put 'raise '(complex) (lambda (x) (tag (make-term 0 x))))
(put 'raise
     '(term)
     (lambda (x)
       (tag (adjoin-term x (the-empty-termlist)))))
(put 'raise
     '(sparse)
     (lambda (sparselist)
       (tag (make-dense-from-sparse
             (contents sparselist)))))
(put 'raise '(dense) (lambda (x) (tag (make-poly '$ x))))
