#lang racket

;; 变化 和 赋值 本质上就是一件事，我们在变化一个什么东西的时候就是在赋值
;; 也就是在保证其前后 "相同" 的基础上对这个对象做出了改变, 无论是多么复杂的变化，其根源一定是赋值导致的
;; 比如下面的 set-car! 本质上就是赋值了这个过程中对应的 x 罢了
(define (cons x y)
  (define (set-x! v)
    (set! x v))
  (define (set-y! v)
    (set! y v))
  (define (dispatch m)
    (cond
      [(eq? m 'car) x]
      [(eq? m 'cdr) y]
      [(eq? m 'set-car!) set-x!]
      [(eq? m 'set-cdr!) set-y!]
      [else (error "Undefined operation -- CONS" m)]))
  dispatch)

(define (car z)
  (z 'car))
(define (cdr z)
  (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define hello (cons 2 3))
(car hello)
(cdr hello)

(set-car! hello 'a)
(car hello)
(set-cdr! hello 'b)
(cdr hello)
