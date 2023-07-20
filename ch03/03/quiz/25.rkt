#lang sicp

;; 推广 table 到任意维度
;; 维数的上升本质上是 record 层数上升
;; 原来是 *table* -> *math* -> (a, b ,c) ...
;; 后来是 *table* -> *math* -> a -> (1 2 3) ...
;; 想要实现 (a, b) 和 (a, b, c) 都可以存储值，我们需要多一个字段来存放 (a, b) 的值, 而不是默认 (a, b) 只能指向 c
;; (cons key value|nextitem) is not enough
;; we need seperated hook for them
;; first
;; (list *table* '())
;; then set (a,b) -> 1
;; (list *table* '() (list a '() (list b 1)))
;; then set (a) -> 1
;; (list *table* '() (list a 1 (list b 1)))
;; then set (a,c) -> 2
;; (list *table* '() (list a 1 (list b 1) (list c 2)))

(define (make-table table-key same-key?)
  (define (make-record key value subtable)
    (list key value subtable))
  (define (record-key record)
    (car record))
  (define (record-value record)
    (cadr record))
  (define (record-subtable record)
    (cddr record)
    #| (let ([subtable (cddr record)]) |#
    #|   (if (null? subtable) subtable (car subtable))) |#)
  (define (set-record-value record value)
    (set-car! (cdr record) value))
  (define (set-record-subtable record subtable)
    (set-cdr! (cdr record) subtable))

  (let ([local-table (make-record '*table* '() '())])

    ;; 只是在一层里寻找，不会进入下一层
    (define (assoc key records)
      (if (null? records)
          #f
          ;; todo find function
          (let ([check-record (car records)])
            (if (null? check-record)
                #f
                (cond
                  [(same-key? key (record-key check-record))
                   check-record]
                  [else
                   (assoc key (cdr records))])))
          )
      )

    (define (lookup . keys)
      (define (look-iter table keys)
        (let ([current-key (car keys)]
              [remain-keys (cdr keys)])
          (let ([record (assoc current-key
                               (record-subtable table))])
            (if record
                ;; 递归处理直到没有元素, 此时说明已经到了最后一层的 cons, 获得 cdr 即目标值
                (if (null? remain-keys)
                    (if (null? (record-value record))
                        #f
                        (record-value record))
                    (look-iter record remain-keys))
                #f))))
      (look-iter local-table keys))

    (define (insert! value . keys)
      (define (insert-iter! value table keys)
        (let ([current-key (car keys)]
              [remain-keys (cdr keys)]
              [subtable (record-subtable table)])
          (let ([record (assoc current-key subtable)])
            (cond
              [(and record)
               ;; 1. has record, no remain keys: has attened to the end layer, update record
               ;; this procedure might be the subpart of outlayer function
               ;; we need to return the table for each possibilities
               (if (null? remain-keys)
                   (begin
                     (set-record-value record value)
                     table)
                   ;; 2. has record, has remain keys: still got sub table: get inside
                   (insert-iter! value record remain-keys))]
              ;; 3. no record, no remain-keys, has attened to the end layer, create a new record and insert into the front of table
              [else
               (if (null? remain-keys)
                   (begin
                     (set-record-subtable
                      table
                      (cons
                       (make-record current-key value '())
                       subtable))
                     table)
                   ;; 4.no record, has remain-keys, insert remain-keys into a new generated table, it will be a recursive procedure too
                   (begin
                     (set-record-subtable
                      table
                      (cons (insert-iter!
                             value
                             (list current-key '() '())
                             remain-keys)
                            subtable))
                     table))]))))
      (insert-iter! value local-table keys)
      local-table)

    (define (dispatch m)
      (cond
        [(eq? m 'lookup-proc) lookup]
        [(eq? m 'insert-proc!) insert!]
        [else (error "Unknown operation -- TABLE" m)]))
    dispatch))

(define operational-table (make-table '*table* equal?))
(define get (operational-table 'lookup-proc))
(define put (operational-table 'insert-proc!))

(put 1 'a 'b 'c)
(put 2 'a 'b 'd)
(put 3 'a 'b)
(put 4 'a)

(get 'a)
(get 'a 'b)
(get 'a 'b 'c)
(get 'a 'b 'd)
(get 'x)
