#lang sicp

(define (call-each procedures)
  (if (null? procedures)
      'call-each-done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (make-wire)
  (let ([signal-value 0] [action-procedures '()])
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'make-wire-done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond
        [(eq? m 'get-signal) signal-value]
        [(eq? m 'set-signal) set-my-signal!]
        [(eq? m 'add-action!) accept-action-procedure!]
        [else (error "Unknown Operation: WIRE" m)]))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire value)
  ((wire 'set-signal) value))
(define (add-action! wire action)
  ((wire 'add-action!) action))

(#%provide make-wire get-signal set-signal! add-action!)
