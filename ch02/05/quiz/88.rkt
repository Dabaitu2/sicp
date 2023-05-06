#lang sicp

;; api.rkt
(define (negate x) (apply-generic 'negate x))

;; integer.rkt
(put 'negate '(integer) (lambda (x) (tag (- x))))

;; rational.rkt
(put 'negate
     '(rational)
     (lambda (x) (make-rat (- (numer x))
                           (- (denom x)))))

;; real.rkt
(put 'negate
     '(real)
     (lambda (x) (tag (- x))))

;; complex.rkt
(put 'negate
     '(complex)
     (lambda (z) (make-from-real-imag (- (real-part z))
                                      (- (imag-part z)))))

;; poly.rkt
(define (negate-terms termlist)
  (if (empty-termlist? termlist)
      the-empty-termlist
      (let ((t (first-term termlist)))
        (adjoin-term (make-term (order t) (negate (coeff t)))
                     (negate-terms (rest-terms termlist))))))

(define (negate poly)
  (make-polynomial (variable poly)
                   (negate-terms (term-list poly))
                   ))

(define (sub-poly p1 p2)
  (add-poly p1 (negate p2)))

(put 'sub
     '(polynomial polynomial)
     (lambda (p1 p2) (tag (sub-poly p1 p2))))

(put 'negate
     '(polynomial)
     negate)
