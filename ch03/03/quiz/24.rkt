#lang sicp

(define (make-table same-key?)
  (let ([local-table (list '*table*)])
    (define (assoc key records)
      (cond
        [(null? records) false]
        ;; equal can compare multiple type of values: symbol | datum | list
        [(same-key? key (caar records)) (car records)]
        [else (assoc key (cdr records))]))
    (define (lookup key-1 key-2)
      (let ([subtable (assoc key-1 (cdr local-table))])
        (if subtable
            (let ([record (assoc key-2 (cdr subtable))])
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ([subtable (assoc key-1 (cdr local-table))])
        (if subtable
            (let ([record (assoc key-2 (cdr subtable))])
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            ;; 如果一维都没有就在二维表最前面插入一个新的一维表
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        [(eq? m 'lookup-proc) lookup]
        [(eq? m 'insert-proc!) insert!]
        [else (error "Unknown operation -- TABLE" m)]))
    dispatch))

(define operational-table (make-table equal?))
(define get (operational-table 'lookup-proc))
(define put (operational-table 'insert-proc!))

(put 'a 'b 1)
(get 'a 'b)
