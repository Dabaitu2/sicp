#lang sicp

(#%require "./connector.rkt")

;; adder 就是一个 constraint unit
(define (adder a1 a2 sum)
  ;; 执行时 me 才会被从环境中求值，这也是为什么这么写不出错的原因
  (define (process-new-value)
    (cond
      [(and (has-value? a1) (has-value? a2))
       (set-value! sum
                   (+ (get-value a1) (get-value a2))
                   me)]
      [(and (has-value? a1) (has-value? sum))
       (set-value! a2
                   (- (get-value sum) (get-value a1))
                   me)]
      [(and (has-value? a2) (has-value? sum))
       (set-value! a1
                   (- (get-value sum) (get-value a2))
                   me)]))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond
      [(eq? request 'I-have-a-value) (process-new-value)]
      [(eq? request 'I-lost-my-value)
       (process-forget-value)]
      [else (error "Unknown request: ADDER" request)]))

  ;; me 会被 connect 到别的 connector 上
  ;; 别的 connector 会通过 request 去通知它值的变化，同时会通过 process-new-value
  ;; set-value! 到别的 connector 上
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

;; multiplier 的实现和 adder 没啥区别，就是 process-new-value 的执行细节不同
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
      [(or (and (has-value? m1) (= (get-value m1) 0))
           (and (has-value? m2) (= (get-value m2) 0)))
       (set-value! product 0 me)]
      [(and (has-value? m1) (has-value? m2))
       (set-value! product
                   (* (get-value m1) (get-value m2))
                   me)]
      [(and (has-value? product) (has-value? m1))
       (set-value! m2
                   (/ (get-value product) (get-value m1))
                   me)]
      [(and (has-value? product) (has-value? m2))
       (set-value! m1
                   (/ (get-value product) (get-value m2))
                   me)]))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond
      [(eq? request 'I-have-a-value) (process-new-value)]
      [(eq? request 'I-lost-my-value)
       (process-forget-value)]
      [else (error "Unknown request: MULTIPLIER" request)]))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTATNT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

;; probe 本身也是一种 constraint
(define (probe name connector)
  (define (print-probe value)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond
      [(eq? request 'I-have-a-value) (process-new-value)]
      [(eq? request 'I-lost-my-value)
       (process-forget-value)]
      [else (error "Unknown request: PROBE" request)]))
  (connect connector me)
  me)

(#%provide constant probe multiplier adder)
