#lang sicp

(define coercion-list '())

(define (clear-coercion-list)
  (set! coercion-list '()))

(define (put-coercion type1 type2 item)
  (if (get-coercion type1 type2)
      coercion-list
      (set! coercion-list
            (cons (list type1 type2 item) coercion-list))))

(define (get-coercion type1 type2)
  (define (get-type1 listItem)
    (car listItem))
  (define (get-type2 listItem)
    (cadr listItem))
  (define (get-item listItem)
    (caddr listItem))
  (define (get-coercion-iter list type1 type2)
    (if (null? list)
        #f
        (let ([top (car list)])
          (if (and (equal? type1 (get-type1 top))
                   (equal? type2 (get-type2 top)))
              (get-item top)
              (get-coercion-iter (cdr list) type1 type2)))))
  (get-coercion-iter coercion-list type1 type2))

(define (make-table)
  (let ([local-table (list '*table*)])
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
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'OK)
    (define (dispatch m)
      (cond
        [(eq? m 'lookup-proc) lookup]
        [(eq? m 'insert-proc) insert!]
        [else (error "Unknown operation -- TABLE" m)]))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))

(#%provide get put put-coercion get-coercion)
