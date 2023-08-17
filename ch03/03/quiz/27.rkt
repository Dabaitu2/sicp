#lang sicp
(#%require "../../../common/data/table.rkt")

;; memorization vs normal way to compute fibonacci
(define (fib n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

(define (make-basic-table)
  (make-table '*table = > <))

(define (memorize f)
  (let ([table (make-basic-table)])
    (lambda (x)
      (let ([previously-computed-result
             ((table 'lookup-proc) x)])
        (or previously-computed-result
            (let ([result (f x)])
              ((table 'insert-proc!) result x)
              result))))))

(define memo-fib
  (memorize
   (lambda (n)
     (cond
       [(= n 0) 0]
       [(= n 1) 1]
       [else (+ (memo-fib (- n 1)) (memo-fib (- n 2)))]))))

(define memo-fib-wrong
  (memorize fib))

(memo-fib 3)

;; that would not work (to shrink computation time cuz it will recursive invoke it
;; self rather than memorized function (memo-fib))
;; (in else clause)

(memo-fib-wrong 3)
#| (fib 3) |#

