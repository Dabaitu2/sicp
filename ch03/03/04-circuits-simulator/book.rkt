#lang sicp
;; Follow the book sequences

;; Simulate circuit
;; Basic Element:
;; 1. wire -> connecting elements (primarily or complex one)
;; 2. primitive element -> and gate(&&), or gate(|), invertor(-)
;; 3. complex boxes construct by 1 & 2

;; We start from wishful thinking, what will our program's usage method be like?
;; we can construct a *wire* by make-wire
;; wire can `hold` a *signal*
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

;; we can conncet wire to a element constructor to create primitive element
(or-gate a b d) ;; ok
(and-gate a b c) ;; ok
(inverter c e) ;; ok
(and-gate d e s) ;; ok

;; after it, we should achieve the half adder.
;; it get two input Signal from a, b, generate two output Signal to s, c
;; S -> if one of A or B *precisely* be 1, then S -> 1 (S -> sum) 当前位的和输出
;; C -> if both A and B are 1, C -> 1 (C -> Carry out) 进位信号
;; Logically very easy to understand: adding binary numbers will result in the following results
;;
;; 1 1 -> current bit sum -> 0, Cout -> 1
;; 1 0 / 0 1 ->  sum 1, Cout 0
;; 0 0 -> sum 0, Cout 0
;;
;; half-adder can only accept two input, cannot get cout from last bit, that's why it called 'half'
;;
;;
;;             ┌───────────────────────────────────────────────────────┐
;;             │  half-adder                                           │
;;             │                                                       │
;;             │                                                       │
;;     A       │        ┌──────────┐D                  ┌──────────┐    │
;;   ──────────┼───┬────┤          ├───────────────────┤          │    │
;;             │   │    │ or-gate  │                   │          ├────┼──────── S
;;             │ ┌─┼────┤          │                 ┌─┤ and gate │    │
;;             │ │ │    └──────────┘   ┌──────────┐  │ │          │    │
;;             │ │ │                   │          │  │ └──────────┘    │
;;             │ │ │                   │ inverter ├──┘                 │
;;             │ │ │                 ┌─┤          │E                   │
;;             │ │ │ ┌───────────┐   │ └──────────┘                    │
;;             │ │ └─┤           │   │                                 │
;;    B        │ │   │ and-gate  │   │                                 │
;;   ──────────┼─┴───┤           ├───┴─────────────────────────────────┼──────── C
;;             │     └───────────┘                                     │
;;             │                                                       │
;;             └───────────────────────────────────────────────────────┘

(define (half-adder a b s c)
  (let ([d (make-wire)] [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; by half-adder & or-gate we can achive full-adder
;; how to explain the circuit
;; 1. by half-add  b and C_in, we can get if the current_bit might be Carry out(Cout1),
;; and what the current bit will be temporarily
;; 2. then we add the current bit with a, will get the final result for current bit, which is SUM
;; and we will get another Carry out Signal (Cout2), according to true value table

;; we could know that in each step the Cout result might be 1, so we use a Or-gate to get the final Cout Result
;; and the final SUM also has been confirm
;;
;;
;;
;;
;;
;;                    ┌────────────────────────────────────────────────────────────┐
;;                    │  fall-adder                                                │
;;                    │                                                            │
;;                    │                                ┌─────────────┐             │
;;            A       │                                │             │             │
;;          ──────────┼────────────────────────────────┤             │             │
;;                    │                                │             ├─────────────┼─── SUM
;;                    │                                │  half-adder │             │
;;                    │                                │             │             │
;;                    │                                │             │             │
;;                    │         ┌───────────────┐ s    │             │ c2 ┌─────┐  │
;;           B        │         │               ├──────┤             ├────┤     │  │
;;          ──────────┼─────────┤               │      └─────────────┘    │ or  │  │
;;                    │         │  half-adder   │ c1                      │gate ├──┼───── Cout
;;           Cin      │         │               ├─────────────────────────┤     │  │
;;          ──────────┼─────────┤               │                         └─────┘  │
;;                    │         └───────────────┘                                  │
;;                    │                                                            │
;;                    └────────────────────────────────────────────────────────────┘
;;

(define (fall-adder a b c-in sum c-out)
  (let ([s (make-wire)] [c1 (make-wire)] [c2 (make-wire)])
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

;; Now we can start to implement the primitive function boxes
(define (get-signal wire)
  ;; (display "TODO")
  (wire 'get-signal))
(define (set-signal! wire value)
  ;; (display "TODO")
  ((wire 'set-signal) value))

;; if the signal of the wire changes, the action nned to be excecuted
;; this type of action will help to transfer the change of that signal to other connected wires
(define (add-action! wire action)
  #| (display "TODO") |#
  ((wire 'add-action!) action))

;; execute the proc after delay specified time
#| (define (after-delay delay proc) |#
#|   (display "TODO")) |#

;; achieve inverter
(define (inverter input output)
  ;; define inner procedure
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
  (define (and-action-procedure)
    (let ([new-value (logical-and (get-signal a1)
                                  (get-signal a2))])
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  ;; either change of input signal will cause output change
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

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

;; Now we start to simulate Wires
;; Wire is actually a pub-sub like object
;; it will hold a local state (signal-value)
;; and a list of procedures (actions)
;; if signal has changed, all the procedures needs to be executed
;; we use message-passing to do that
(define (make-wire)
  (let ([signal-value 0] [action-procedures '()])
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures)))
    (define (dispatch m)
      (cond
        [(eq? m 'get-signal) signal-value]
        [(eq? m 'set-signal) set-my-signal!]
        [(eq? m 'add-action!) accept-action-procedure!]
        [else (error "Unknown Operation: WIRE" m)]))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

;; now we need to achive `delay`, it requires us to have a overview of the sequence and time of happening of each thing of it
;; and it can schedue all the actions. like a runtime event queue?
;; we use the data structure `Agenda` to do so
#| • (make-agenda) returns a new empty agenda. |#
#| • (empty-agenda? ⟨agenda⟩) is true if the specified agenda is empty. |#
#| • (first-agenda-item ⟨agenda⟩) returns the first item on the agenda. |#
#| • (remove-first-agenda-item! ⟨agenda⟩) modifies the agenda by removing the first item. |#
#| • (add-to-agenda! ⟨time⟩ ⟨action⟩ ⟨agenda⟩) modifies the agenda by adding the given action procedure to be run at the spec- ified time. |#
#| • (current-time ⟨agenda⟩) returns the current simulation time. |#

;; still, wishful thinking first
(define (make-agenda)
  (display "TODO"))
(define (empty-agenda? agenda)
  (display "TODO"))
(define (first-agenda-item agenda)
  (display "TODO"))
(define (add-to-agenda! time action agenda)
  (display "TODO"))
(define (remove-first-agenda-item! agenda)
  (display "TODO"))
(define (current-time agenda)
  (display "TODO"))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

;; we will trigger propagate to tick start the time flow
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ([first-item (first-agenda-item the-agenda)])
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; sample simulation: probe
;; it just display the signal value if it change
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
