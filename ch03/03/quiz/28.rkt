#lang sicp

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ([new-value (logical-or (get-signal o1)
                                 (get-signal o2))])
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  ;; either change of input signal will cause output change
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure))

(define (logical-or a1 a2)
  (if (or (= a1 1) (= a2 1)) 1 0))
