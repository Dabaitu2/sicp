#lang racket
(#%require "./tag-tools.rkt")
(#%require "./env.rkt")
(#%require "./coercion.rkt")
(#%require "./api.rkt")

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real) +)
  (put 'sub '(real real) -)
  (put 'mul '(real real) *)
  (put 'div '(real real) /)
  (put 'negate '(real) (lambda (x) (tag (- x))))
  (put 'equ? '(real real) =)
  (put 'less? '(real real) <)
  (put 'more? '(real real) >)
  (put '=zero? '(real) (lambda (x) (= x 0)))
  (define (reduce-reals n d)
    (let ([g (gcd n d)]) (list (/ n g) (/ d g))))

  #| (put 'project |#
  #|      '(real) |#
  #|      (lambda (x) (attach-tag 'rational (cons (round x) 1)))) |#

  (put 'cosine '(real) (lambda (x) (tag (cos x))))
  (put 'sine '(real) (lambda (x) (tag (sin x))))

  (put 'raise
       '(rational)
       (lambda (x)
         (tag (div ((get 'numer '(rational)) (contents x))
                   ((get 'denom '(rational)) (contents x))))))

  (put 'reduce
       '(real real)
       (lambda (a b)
         (map (lambda (x) (tag x)) (reduce-reals a b))))

  (put 'greatest-common-divisor '(real real) gcd)
  (put 'make
       'real
       (lambda (x)
         (if (number? x)
             x
             (error "arg is not number -- make-real" x))))
  'done)

(define (make-real n)
  ((get 'make 'real) n))
(define (rational->real n)
  (make-real
   (/ (apply (get 'numer '(rational)) (list (contents n)))
      (apply (get 'denom '(rational))
             (list (contents n))))))
(put-coercion 'rational 'real rational->real)

(#%provide install-real-package make-real)
