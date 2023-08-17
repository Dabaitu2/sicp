#lang sicp

(#%require "../04-circuits-simulator/wire.rkt")
(#%require "../04-circuits-simulator/agenda.rkt")
(#%require "../04-circuits-simulator/units.rkt")


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

(set-signal! input-1 0)
(set-signal! input-2 1)
(and-gate input-1 input-2 sum)
(probe 'sum sum)
(propagate)
(set-signal! input-1 1)
(set-signal! input-2 0)
(propagate)


;; input-1 0 -> 1 new_value: 1 after-delay put set-signal! callback after 3 unit of time
;; input-2 1 -> 0, new_value: 0, after-delay put set-signal! callback after 3 unit of time
;; generally, we want the result should be 0, 
;; but if our execute sequnence of the procedures changes to LIFO (Last in First Out) rather than FIFO queue
;; in our and-gate case, then because of the new=value has been calculated and stored in advance (the sequence of get logical-and was unchanged)
;; the output will become 1, it's not following our assumption
;; so we have to make first value to be outputed, cuz the temp value will be executed as FIFO sequence (function's invokaction sequence)
