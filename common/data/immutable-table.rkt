#lang sicp

(define (make-table)
  '()) ; 初始空表

(define (assoc key records)
  (cond
    [(null? records) #f]
    [(equal? key (caar records)) (car records)]
    [else (assoc key (cdr records))]))

(define (lookup key table)
  (let ([record (assoc key table)])
    (if record
        (cdr record) ; 返回与键关联的值
        #f))) ; 没有找到

(define (insert! key value table)
  ;; 如果key已存在，则创建一个不包含该key的新表，并添加新键值对
  ;; 否则直接在前面添加新键值对
  ;; 这样保证了table没有被修改，而是返回了一个新table
  (let ([record (assoc key table)])
    (if record
        ;; 如果记录存在，先删除旧记录再插入新记录
        (cons `(,key . ,value) (remove-record key table))
        ;; 如果记录不存在，直接插入新记录
        `(,(cons key value) . ,table))))

(define remove-record
  ;; 移除与给定键相关联的记录，并返回结果表格。
  ;; 这个函数也是纯函数，并不改变原始表。
  (lambda (key alist)
    (cond
      [(null? alist) '()]
      [(equal? key (caar alist))
       (remove-record key (cdr alist))]
      [else
       (cons (car alist)
             (remove-record key (cdr alist)))])))

;; 使用示例:

(let ([table1 (make-table)])
  ; 创建并插入几个元素到表格中。
  ; 注意我们必须绑定每次插入后返回的新表以跟踪其状态。
  ; 这是因为我们没有修改原始表。
  ; 在实际应用中，可能需要将最新状态持续传递下去。

  ;; 插入元素时注意重新捕获生成的新表
  ; 创建并插入几个元素到表格中。
  ; 注意我们必须绑定每次插入后返回的新表以跟踪其状态。
  ; 这是因为我们没有修改原始表。

  (let* ([table2 (insert! 'a1 'value1 table1)]
         [table3 (insert! 'a2 'value2 table2)]
         [table4 (insert! 'a3 'value3 table3)])
    (display (lookup 'a1 table4))
    (newline) ;输出value1
    (display (lookup 'a2 table4))
    (newline) ;输出value2
    (display (lookup 'a3 table4))
    (newline))) ; 输出value3
