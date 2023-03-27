#lang sicp

;; gcd: 如果 r 是 a / b 的余数，则 a 和 b 的公约数正好也是 b 和 r 的公约数
;; gcd(a, b) = gcd(b, r)
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ([g ((if (< d 0) - +) (abs (gcd n d)))])
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 1 -2))
(print-rat (make-rat 6 -9))


