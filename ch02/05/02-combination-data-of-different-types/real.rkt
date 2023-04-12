#lang sicp
(#%require "./tag-tools.rkt")
(#%require "./env.rkt")
(#%require "./coercion.rkt")

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real) +)
  (put 'sub '(real real) -)
  (put 'mul '(real real) *)
  (put 'div '(real real) /)
  (put 'equ? '(real real) =)
  (put '=zero? '(real) (lambda (x) (= x 0)))

  (put 'project '(real) (lambda (x) ('integer (round x))))
  (put 'raise
       '(rational)
       (lambda (x) (tag (/ (apply (get 'numer '(rational)) (list (contents x)))
                           (apply (get 'denom '(rational)) (list (contents x)))))))

  (put 'make 'real (lambda (x)
                     (if (number? x)
                         x
                         (error "arg is not number -- make-real" x))))
  'done)

(define (make-real n)
  ((get 'make 'real) n))
(define (rational->real n)
  (make-real (/ (apply (get 'numer '(rational)) (list (contents n)))
                (apply (get 'denom '(rational)) (list (contents n))))))
(put-coercion 'rational
              'real
              rational->real)

(#%provide install-real-package make-real)