#lang sicp

;; use and-gate with inverter acheive or-gate
;; helpers
(define (get-signal wire)
  (display "TODO"))
(define (set-signal! wire)
  (display "TODO"))
;; if the signal of the wire changes, the action nned to be excecuted
;; this type of action will help to transfer the change of that signal to other connected wires
(define (add-action! wire action)
  (display "TODO"))
;; execute the proc after delay specified time
(define (after-delay delay proc)
  (display "TODO"))

(define (inverter input output)
  ;; define inner procedure
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay
       inverter-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input))

(define (logical-not s)
  (cond
    [(= s 0) 1]
    [(= s 1) 0]
    [else (error "Invalid signal" s)]))

(define (add-gate a1 a2 output)
  (define (and-action-procedure)
    (let ([new-value (logical-and (get-signal a1)
                                  (get-signal a2))])
      (after-delay
       add-gate-delay
       (lambda () (set-signal! output new-value)))))
  ;; either change of input signal will cause output change
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

(define (logical-and a1 a2)
  (if (and (= a1 1) (= a2 1)) 1 0))

;; 1 0 1
;; 1 1 1
;; 0 1 1
;; 0 0 0
;; Our Code
(define (or-gate o1 o2 output)
  (let ([no1 (make-wire)]
        [no2 (make-wire)]
        [nout (make-wire)])
    (inverter o1 no1)
    (inverter o2 no2)
    (and-gate no1 no2 nout)
    (inverter nout output)))

(define (logical-or a1 a2)
  (if (or (= a1 1) (= a2 1)) 1 0))
