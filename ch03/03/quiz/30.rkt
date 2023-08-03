#lang sicp

;; achieve ripple-carry-adder

;; helpers
(define (make-wire)
  (display "OMITTED"))
(define (fall-adder a b c-in sum c-out)
  (display "OMITTED"))
(define (set-signal! wire)
  (display "TODO"))

(define (ripple-carry-adder alist blist slist c-in)
  (if (or (null? alist) (null? blist) (null? slist))
      (set-signal! c-in 0)
      (let ([c-temp (make-wire)])
        (ripple-carry-adder (cdr alist)
                            (cdr blist)
                            (cdr slist)
                            c-temp)
        ;; c-in will be left for set-signal! to 0 in the end
        ;; and we are just create a lot of c-temp and use them as conncetor of FAs 
        (fall-adder (car alist)
                    (car blist)
                    c-temp
                    (car slist)
                    c-in))))

;; half-adder delay
;; 1 or-delay
;; 2 and-delay
;; 1 inverter-delay

;; fall-adder delay
;; 2 half-adder
;; 1 or-delay

;; ripple-carry-adder
;; n * fall-adder delay
