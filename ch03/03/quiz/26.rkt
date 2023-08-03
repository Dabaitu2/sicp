#lang sicp

;; 原先我们使用的表格底层实现是基于 unordered-set, 这在面对数据极大的 table 时性能也许不太好 (因为扫描的时间复杂度是 O(N))
;; 而如果可以将 table 的设计由 unordered-set, 转换成类似二叉查找树的话, 那么也许可以很大程度上实现搜索性能的提升 (O(LogN))
;; !!!! 这就是最最本质的 RDS/字典 了 !!!!! 我们此时就可以理解为什么数据库的底层是一颗树
;; 这里我们先提供对于数字类型的存储实现
;; 理论上，只要提供 gt, lt, eq 三类判断函数，我们可以支持各种类型的二叉树判断
;; 基础设计：
;; record: ((key value subtable) left right)
;; 只需要将 3.25 中的 subtable 换成 subtree 即可
;; 和 key 同 size 的节点会在同一颗树, 而比 key size 多的节点会在 subtree 中进一步处理

;; helpers
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

;; constructor 前序遍历
(define (make-tree entry left right)
  (list entry left right))

(define (make-record key value subtable)
  (make-tree (list key value subtable) '() '()))
(define (record-key record)
  (caar record))
(define (record-value record)
  (cadar record))
(define (record-subtable record)
  (cddar record))

(define (set-record-value record value)
  (set-car! (cdar record) value))
(define (set-record-subtable record subtable)
  (set-cdr! (cdar record) subtable))

;; main logic
(define (make-table table-key key-eq? key-gt? key-lt?)

  (let ([local-table (make-record table-key '() '())])

    (define (adjoin-set x set)
      (if (or (null? set) (null? (entry set)))
          ;; !! IMPORTANT:
          ;; 因为 x 自身就是树结构, 此处的 adjoin 并不需要在 set 不存在的时候再创建一层子结构了
          ;; 而是直接利用 x 去替换
          x
          (let ([set-entry-key (record-key set)]
                [set-entry (entry set)]
                [x-key (record-key x)])
            (cond
              [(key-eq? x-key set-entry-key) set]
              [(key-lt? x-key set-entry-key)
               (make-tree set-entry
                          (adjoin-set x (left-branch set))
                          (right-branch set))]
              [(key-gt? x-key set-entry-key)
               (make-tree
                set-entry
                (left-branch set)
                (adjoin-set x (right-branch set)))]))))

    ;; 这里本质上就是二叉树查找
    (define (assoc key records)
      (if (or (null? records) (null? (entry records)))
          #f
          (cond
            [(key-eq? key (record-key records)) records]
            [(key-lt? key (record-key records))
             (assoc key (left-branch records))]
            [(key-gt? key (record-key records))
             (assoc key (right-branch records))])))

    (define (lookup . keys)
      (define (look-iter table keys)
        (let ([current-key (car keys)]
              [remain-keys (cdr keys)])
          (let ([record (assoc current-key
                               (record-subtable table))])
            (if record
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
          ;; check 的是 subtable ! 不是当前的根所在的这层树, 当前的根已经在上层中排查完毕了
          (let ([record (assoc current-key subtable)])
            (if record
                ;; 1. has record, no remain keys: has attened to the end layer, update record
                ;; this procedure might be the subpart of outlayer function
                ;; we need to return the table for each possibilities
                (if (null? remain-keys)
                    (begin
                      (set-record-value record value)
                      table)
                    ;; 2. has record, has remain keys: still got sub table: get inside
                    (insert-iter! value record remain-keys))
                ;; 3. no record, no remain-keys, has attened to the end layer, create a new record and insert into the front of table
                (if (null? remain-keys)
                    (begin
                      (set-record-subtable
                       table
                       (adjoin-set
                        (make-record current-key value '())
                        subtable))
                      table)
                    ;; 4.no record, has remain-keys, insert remain-keys into a new generated table,
                    ;; it will be a recursive procedure too
                    (begin
                      (set-record-subtable
                       table
                       (insert-iter!
                        value
                        (make-record current-key '() '())
                        remain-keys))
                      table))))))
      (insert-iter! value local-table keys)
      local-table)

    (define (dispatch m)
      (cond
        [(eq? m 'lookup-proc) lookup]
        [(eq? m 'insert-proc!) insert!]
        [else (error "Unknown operation -- TABLE" m)]))
    dispatch))

(define (key-eq? a b)
  (string=? (symbol->string a) (symbol->string b)))

(define (key-lt? a b)
  (string<? (symbol->string a) (symbol->string b)))

(define (key-gt? a b)
  (string>? (symbol->string a) (symbol->string b)))

(define operational-table
  (make-table '*table* key-eq? key-gt? key-lt?))
(define get (operational-table 'lookup-proc))
(define put (operational-table 'insert-proc!))

(put 1 'a 'b 'c)
(put 2 'a 'b 'd)
(put 3 'a 'b)

(get 'a)
(get 'a 'b)
(get 'a 'b 'c)
(get 'a 'b 'd)
(get 'x)
