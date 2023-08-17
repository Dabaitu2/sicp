#lang sicp
(#%require "./wire.rkt")
(#%require "./agenda.rkt")

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (inverter input output)
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input))

(define (logical-not s)
  (cond
    [(= s 0) 1]
    [(= s 1) 0]
    [else (error "Invalid signal" s)]))

(define (and-gate a1 a2 output)
  ;; 此过程最终目的是为 agenda 在适当的时间 (当前电路所需要的总时间) 后将 output 的 signal 改变
  ;; agenda 就是一个自动化程序，在 propagate 后调整 signal 并更新当前时间
  (define (and-action-procedure)
    (let ([new-value (logical-and (get-signal a1)
                                  (get-signal a2))])
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  ;; either change of input signal will cause output change
  ;; add-action 目的是将过程加入 input wire 的回调中, 当 a1 的值变化时就会触发回调
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'and-gate-ok)

(define (logical-and a1 a2)
  (if (and (= a1 1) (= a2 1)) 1 0))

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ([new-value (logical-or (get-signal o1)
                                 (get-signal o2))])
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  ;; either change of input signal will cause output change
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure))

(define (logical-or a1 a2)
  (if (or (= a1 1) (= a2 1)) 1 0))

(define (half-adder a b s c)
  (let ([d (make-wire)] [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'half-adder-ok))

(define (fall-adder a b c-in sum c-out)
  (let ([s (make-wire)] [c1 (make-wire)] [c2 (make-wire)])
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

(#%provide inverter
           inverter-delay
           and-gate
           and-gate-delay
           or-gate
           or-gate-delay
           half-adder
           fall-adder)
