#lang sicp
(#%require "../../../common/data/table.rkt")

(define (key-eq? a b)
  (string=? (symbol->string a) (symbol->string b)))
(define (key-lt? a b)
  (string<? (symbol->string a) (symbol->string b)))
(define (key-gt? a b)
  (string>? (symbol->string a) (symbol->string b)))

;; check if the key was set in records
(define (assoc key records)
  (cond
    [(null? records) #f]
    [(equal? key (caar records)) (car records)]
    [else (assoc key (cdr records))]))

(define my-table
  (make-table '*table* key-eq? key-gt? key-lt?))

(define get (my-table 'lookup-proc))
(define put (my-table 'insert-proc!))
(define print-table (my-table 'check-proc))

(#%provide assoc get put print-table)
