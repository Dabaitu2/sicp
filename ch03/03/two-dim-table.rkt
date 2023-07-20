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

;; two-dimensional table
;; 二维表中的每个值由两个 key 去 index
;; 我们要把所有具有相同第一个 key 的元素 (其实就是一张一维表格) 聚合到一起
;; (这里一维表的表头就不再是 *table* 这个 dummy value 而直接是第一个 key)
;; 形成一张二维表

;; 查询 先查 key-1 有没有 subtable, 如果有就查 key-2
(define (lookup key-1 key-2 table)
  (let ([subtable (assoc key-1 (cdr table))])
    (if subtable
        (let ([record (assoc key-2 (cdr subtable))])
          (if record (cdr record) false))
        false)))

;; key1 key2 均有就和一维表一样修改最终的 cons record
;; key1 有 key2 无就同一维表一样在一维表最前端插入新 record
(define (insert! key-1 key-2 value table)
  (let ([subtable (assoc key-1 (cdr table))])
    (if subtable
        (let ([record (assoc key-2 (cdr subtable))])
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        ;; 如果一维都没有就在二维表最前面插入一个新的一维表
        (set-cdr! table
                  (cons (list key-1 (cons key-2 value))
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define tb (make-table))
(insert! 'a 'x 1 tb)
(insert! 'a 'y 2 tb)
(insert! 'b 'y 3 tb)
(lookup 'a 'x tb)
(lookup 'b 'y tb)

