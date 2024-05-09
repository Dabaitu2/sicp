#lang sicp

(#%require "./wire.rkt")
(#%require "./agenda.rkt")
(#%require "./units.rkt")


(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display "[PROBE|TIME]:")
                 (display (current-time the-agenda))
                 (display " [WIRE]:")
                 (display name)
                 (display " [VALUE]:")
                 (display (get-signal wire))
                 (newline))))


(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)
;; (and-gate input-1 input-2 sum)
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
;; 时间流动是由 gate 决定的
(set-signal! input-2 1)
(propagate)
