#lang sicp
(#%require "./connector.rkt")
(#%require "./units.rkt")

;; the general procedure we used is just a one-directional computation
;; like (define c (+ a b)) => c = a + b,
;; we couldn't get value of `a` if we know `b` & `c`, even through we know there do have such relation
;;
;; by putting forward the concept of `constraint`
;; we want to build a system which can describe such relation
;; and can get any part of the relation if other necessary parts are ready
;; so if we know c and a, we can get b immediately
;;
;; constraint system is very similar as our circuit simulator
;; but it's acutally more simple cause we don't need to care about latency and agenda
;;
;; all changes will be put into effect immediately (it's a hagiographic system)
;; an example looks like belowing chart, every large rect is a `constraint` (the primitive unit of our system)
;; and they can be connected by conncetor which looks like the circuit simulator's *wire*
;;
;;
;;
;;                      relationship between Fahrenheit and Celsius temperatures
;;
;;                                       9C = 5(F - 32)
;;
;;
;;             ┌──────────────┐           ┌──────────────┐            ┌──────────────┐
;;             │              │           │              │     v      │              │
;;       C ────┤ m1           │           │           m1 ├────────────┤ a1           │
;;             │              │    u      │              │            │              │
;;             │       *    P ├───────────┤ P     *      │            │       +    s ├──── F
;;             │              │           │              │            │              │
;;         ┌───┤ m2           │           │           m2 ├──┐      ┌──┤ a2           │
;;         │   │              │           │              │  │      │  │              │
;;         │   └──────────────┘           └──────────────┘  │      │  └──────────────┘
;;        w│                                              x │      │ y
;;         │     ┌──────┐                           ┌────┐  │      │  ┌─────┐
;;         └─────┤ 9    │                           │ 5  ├──┘      └──┤ 32  │
;;               └──────┘                           └────┘            └─────┘
;;

;; connector likes wires
(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(celsius-fahrenheit-converter C F)
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

;; tells C that this directive comes from the user.
(set-value! C 25 'user)
;; Probe: Celsius temp = 25
;; Probe: Fahrenheit temp = 77
;; done

;; (set-value! F 212 'user)
;; Error! Contradiction (77 212)
;; cause the constraint system has hold the value for connector C and F

(forget-value! C 'user)
;; we can let the conncetor C forget it's value of 'user, and the related F will be forgot too
;; Probe: Celsius temp = ?
;; Probe: Fahrenheit temp = ?
;; Probe

(set-value! F 212 'user)
;; Probe: Fahrenheit temp = 212
;; Probe: Celsius temp = 100
;; done

;; this system works for both C -> F and F -> C
;; nondirectionality is the distinguishing feature of constraint-based systems
