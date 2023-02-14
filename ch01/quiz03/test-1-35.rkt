#lang sicp

;; 求黄金分割率的值，它是变换 x->1+1/x 的不动点
(define tolerance 0.00001)
(define (display-info guess step)
  (display "Step: ")
  (display step)
  (display " ")

  (display "Guess: ")
  (display guess)
  (newline))
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (let ([next (f guess)])
      (display-info guess step)
      (if (close-enough? guess next)
          (display "\n")
          (try next (+ 1 step)))))
  (try first-guess 1))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)




