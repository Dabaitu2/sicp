#lang sicp

;; one-dimensional table
;; assoc => associate?
;; check if the key was found inside the records
;; recursive
(define (assoc key records)
  (cond
    [(null? records) false]
    ;; equal can compare multiple type of values: symbol | datum | list
    [(equal? key (caar records)) (car records)]
    [else (assoc key (cdr records))]))

;; find the value from table of specified key
(define (lookup key table)
  (let ([record (assoc key (cdr table))])
    (if record (cdr record) false)))

(define (insert! key value table)
  (let ([record (assoc key (cdr table))])
    (if record
        (set-cdr! record value)
        ;; 新插入的记录放到最前面
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define tb (make-table))
(insert! 'a 1 tb)
(insert! 'b 2 tb)
(lookup 'a tb)
