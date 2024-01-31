#lang sicp
(#%require "./achieve-serializer.rkt")


;; suppose we have this procedure which can run its arguments procedure simultaneously
(define (parallel-execute . args)
  (display "TODO"))

(define x 10)

;; if all those procedure can interleave
;; the final possibility will shrink into 5 kinds
;; 1. p1 set is the last step
;;    (1.a) p1 get after p2 set -> 121
;;    (1.b) p1 get before p2 set -> 100
;;    (1.c) p2 set between p1 get1 & get2  -> 110
;; 2. p2 set is the last step
;;    (2.a) p2 get after p1 set -> 101
;;    (2.b) p2 get before p1 set -> 11
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

;; create a serializer, it looks like make all the procdure like a transaction?
;; make it atmomic?
;; 串行化就是创建一些不同的过程集合，并且保证在每个时刻，在任
;; 何一个串行化集合里至多只有一个过程的一
;; 个执行。如果某个集合里有过程正在执行，而另一进程企图执行这个集合里的任何过程时，
;; 它就必须等待到前一过程的执行结束
;; (define (make-serializer)
;;   (display "TODO"))
(define s (make-serializer))

;; parallel-execute
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

(#%provide make-serializer parallel-execute)
