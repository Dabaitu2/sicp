#lang sicp

;; constructor, make polynomial
(define (make-poly)
  (display "todo"))

;; seletors, get variable （like x) ot term-list () from poly
(define (variable v)
  (display "todo"))
(define (term-list v)
  (display "todo"))

;; api
(define (same-variable? v1 v2)
  (display "todo"))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))
