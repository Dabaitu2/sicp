#lang sicp
(#%require "./tag-tools.rkt")
(#%require "./env.rkt")
(#%require "./api.rkt")
(#%require "./coercion.rkt")

(define (install-rational-package)
  ;; inner procedures
  (define (tag x)
    (attach-tag 'rational x))
  (define (numer x)
    (car x))
  (define (denom x)
    (cadr x))
  (define (make-rat n d)
    (reduce n d))
  #| (if (and (integer? n) (integer? d)) |#
  #|     (let ([g (gcd n d)]) (cons (/ n g) (/ d g))) |#
  #|     (cons n d))) |#

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (numer y) (denom x))))

  ;; a flaw: it only support numeric comparsion
  (define (_equ? x y)
    (let ([g1 (gcd (numer x) (denom x))]
          [g2 (gcd (numer y) (denom y))])
      (= (* (/ (numer x) g1) (/ (denom y) g1))
         (* (/ (numer y) g2) (/ (denom x) g2)))))

  (define (less? x y)
    (let ([g1 (gcd (numer x) (denom x))]
          [g2 (gcd (numer y) (denom y))])
      (< (* (/ (numer x) g1) (/ (denom y) g1))
         (* (/ (numer y) g2) (/ (denom x) g2)))))

  (define (more? x y)
    (let ([g1 (gcd (numer x) (denom x))]
          [g2 (gcd (numer y) (denom y))])
      (> (* (/ (numer x) g1) (/ (denom y) g1))
         (* (/ (numer y) g2) (/ (denom x) g2)))))

  (define (=zero? x)
    (equ? (numer x) 0))

  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'add
       '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub
       '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul
       '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div
       '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'negate
       '(rational)
       (lambda (x) (make-rat (- (numer x)) (- (denom x)))))

  ;; it's hard to project a real number (calculated by original cos) into rational
  ;; I do believe there's a approximate way, but it's beyond what we should focus in this chapter
  ;; So I will just put result into real domain
  (put 'cosine
       '(rational)
       (lambda (n)
         (attach-tag 'real (cos (/ (numer n) (denom n))))))
  (put 'cosine
       '(rational)
       (lambda (n)
         (attach-tag 'real (sin (/ (numer n) (denom n))))))

  (put 'equ? '(rational rational) _equ?)
  (put 'less? '(rational rational) less?)
  (put 'more? '(rational rational) more?)
  ;; apply-generic 只能处理 list, 所以给他包一下
  (put '=zero? '(rational) =zero?)
  (put 'raise
       '(integer)
       (lambda (x) (make-rational (contents x) 1)))

  ;; project 会被 apply-generic 使用，所以会被自动解 tag，这里的 x 一定是 content 数据
  ;; 这里的 drop 实现想要完全做到会很麻烦, 因为 rational 不一定 denom 和 numer 是 numeric 的
  ;; 更合理的思路应该是让 rational-function 和 rational 分开，而不应该让有理数中包含代数式
  ;; 但这个复杂度就远远超出我们应该做的部分了
  #| (put 'project |#
  #|      '(rational) |#
  #|      (lambda (x) |#
  #|        (attach-tag 'integer |#
  #|                    (round (/ (numer x) (denom x)))))) |#

  ;; 而 make 并不会被 apply-generic 使用, 所以倒是不用包装
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

;; 这里不能使用 apply-generic 是因为这里的场景下
;; 我们是从原始值生成一个带 tag 的数据
;; 而 apply-generic 处理的已经是带 tag 的数据了
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (integer->rational n)
  (make-rational (contents n) 1))
(put-coercion 'integer 'rational integer->rational)

(#%provide install-rational-package make-rational)
