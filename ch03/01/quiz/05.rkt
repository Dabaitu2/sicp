#lang racket
(#%require "../../../common/math/num-theory.rkt")

;; Monte Carlo integration (蒙特卡洛积分)
;; To estimate the area of certain function curve
;; we can using a more regular shape like rectagular to confine a fixed calculable area S
;; We can calculate the propotion of those random points whose (x, y) meets the function curve's demands and the area S to get the estimated aera and
;; that's the integral of the aera.

;; helper: generate a number between given ranges
(define (random-in-range low high)
  (let ([range (- high low)]) (+ low (random range))))

;; monte-carlo methods
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      [(= trials-remaining 0) (/ trials-passed trials)]
      [(experiment)
       (iter (- trials-remaining 1) (+ trials-passed 1))]
      [else (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))

;; P: predicate
;; x1, x2, y1, y2: rectagular's lower and higher bounds
;;      o-----o (x2, y2)
;;      |     |
;;      o-----o
;; (X1, y1)
;; trials: times of trial
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (is-in-integral-function?)
    (let ([x (random-in-range x1 x2)]
          [y (random-in-range y1 y2)])
      (P x y)))
  (let ([base-area (* (- x2 x1) (- y2 y1))])
    (* base-area
       (monte-carlo trials is-in-integral-function?))))

;; test
(define (circle-test x y)
  (>= 9 (+ (square (- x 5)) (square (- y 7)))))

(estimate-integral circle-test 2 8 4 10 10000) ;; 27-28, while the real aera is close to 28.27
