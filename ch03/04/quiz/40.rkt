#lang sicp
(#%require
 "../01-nature-of-time-in-concurrent-systems/serializer.rkt")

(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))


;; p1 get1, p1 get2, p1 set
;; p2 get1, p2 get2, p2 get3, p2 set


;; 1. p1 get1 -> p2 set -> p1 get2 -> p1 set
;; 2. p1 get1 -> p1 get2 -> p2 set -> p1 set
;; 3. p1 all -> p2 set 
;; 4. p2 get1 -> p1 set -> p2 left
;; 5. p2 get1 -> p2 get2 -> p1 set -> p2 left
;; 6. p2 get -> p1 set -> p2 set
;; 7  p2 all -> p1 all
