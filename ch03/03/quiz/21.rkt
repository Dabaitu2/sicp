#lang sicp

;; 维护此数据结构
(define (front-ptr queue)
  (car queue))
(define (rear-ptr queue)
  (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

;; 通过只展示 front-ptr 就可以正确表示了
(define (print-queue queue)
  (display (front-ptr queue)))

;; 判断是否是空队列根据元数据 pair 来决定
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue)
  (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      ;; front-ptr 指向的是原 list 的第一个 cons, 所以想要获得值还得 car 一下
      (car (front-ptr queue))))

;; 插入队列： 创建新的 pair, 将 rear-ptr 指向的 list 中的 pair -> pair, 同时将 rear-pair 指针指向 pair
(define (insert-queue! queue item)
  (let ([new-pair (cons item '())])
    (cond
      [(empty-queue? queue)
       (set-front-ptr! queue new-pair)
       (set-rear-ptr! queue new-pair)
       (print-queue queue)]
      [else
       (set-cdr! (rear-ptr queue) new-pair)
       (set-rear-ptr! queue new-pair)
       (print-queue queue)])))

;; 出队：将 front-ptr 指向 front-ptr 指向的 pair 的 cdr
(define (delete-queue! queue)
  (cond
    [(empty-queue? queue)
     (error "DELETE! called with an empty queue" queue)]
    [else
     (set-front-ptr! queue (cdr (front-ptr queue)))
     (print-queue queue)]))

(define q1 (make-queue))
(insert-queue! q1 'a)
(newline)
(insert-queue! q1 'b)
(newline)
(delete-queue! q1)
(newline)
(delete-queue! q1)

;; 这里最后的 q1 输出仍然为 (() b)
;; 是因为 rear 没有被修改
;; 而 front 指向了空导致的

