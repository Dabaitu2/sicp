#lang sicp
(#%require "./tag-tools.rkt")
(#%require "./env.rkt")
(#%require "./coercion.rkt")
(#%require "./generic-tools.rkt")
(#%require "./api.rkt")
(#%require "../../../common/math/num-theory.rkt")


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z)
    (car z))
  (define (imag-part z)
    (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (add (square (real-part z))
               (square (imag-part z)))))
  (define (angle z)
    (if (= 0 (real-part z))
        0
        (atan (imag-part z) (real-part z))
        )
    )
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  ;; 假设 put 已经存在
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag
       'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang
       'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; 通过词法作用域，我们规避了重名问题
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z)
    (car z))
  (define (angle z)
    (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (add (square x) (square y))) (if (= x 0)
                                                 0
                                                 (atan y x))))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag
       'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang
       'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; complex 本身也是基于 poler 和 normal 的通用操作形成的
(define (install-complex-package)
  (install-polar-package)
  (install-rectangular-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (mul (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z)
    (attach-tag 'complex z))

  (define (equ? x y)
    (and (= (magnitude x) (magnitude y))
         (= (angle x) (angle y))))

  (define (=zero? x)
    (and (= (magnitude x) 0) (= (angle x) 0)))

  (put 'add
       '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub
       '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul
       '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div
       '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'negate
       '(complex)
       (lambda (z) (make-from-real-imag (- (real-part z))
                                        (- (imag-part z)))))
  (put 'make-from-real-imag
       'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang
       'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)

  (put 'project '(complex) (lambda (x) (attach-tag 'real (real-part x))))
  (put 'raise
       '(real)
       (lambda (x)
         (tag (make-from-real-imag (contents x) 0))))

  ;; complex 类的数据结构是双层的 (cons 'complex (cons 'rectangular contents))
  ;; 因此外界调用 real-part 时会先解析 complex 取得实际内容
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (magnitude z)
  ((get 'magnitude 'complex) z))
(define (angle z)
  ((get 'angle 'complex) z))
(define (real-part z)
  ((get 'angle 'complex) z))
(define (imag-part z)
  ((get 'angle 'complex) z))

;; register coercions
(define (real->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'real
              'complex
              real->complex)

(#%provide install-complex-package
           make-complex-from-mag-ang
           make-complex-from-real-imag)
