#lang racket
(#%require "./tagged.rkt")
(#%require "./normal-form.rkt")
(#%require "./polar-form.rkt")

;; 通过整合不同的 tagged data 实现通用操作
;; tagged type 可能更类似 TS 中的 tagged union 或者 rust 中的 match 操作
(define (real-part z)
  (cond
    [(rectangular? z) (real-part-rectangular (contents z))]
    [(polar? z) (real-part-polar (contents z))]
    [else (error "Unknown type 00 REAL-PART" z)]))

(define (imag-part z)
  (cond
    [(rectangular? z) (imag-part-rectangular (contents z))]
    [(polar? z) (imag-part-polar (contents z))]
    [else (error "Unknown type: IMAG-PART" z)]))

(define (magnitude z)
  (cond
    [(rectangular? z) (magnitude-rectangular (contents z))]
    [(polar? z) (magnitude-polar (contents z))]
    [else (error "Unknown type: MAGNITUDE" z)]))

(define (angle z)
  (cond
    [(rectangular? z) (angle-rectangular (contents z))]
    [(polar? z) (angle-polar (contents z))]
    [else (error "Unknown type: ANGLE" z)]))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))
