#lang sicp

;; 队列是 FIFO 的缓冲结构
;; 我们也可以使用常规的 list 来实现
;; 然而插入数据的时候会被迫扫描整个表, 因此我们可以再额外用一个数据结构再去指向原 list 头尾, 方便出队和入队
;;
;;
;;            ┌───────┬────────┐
;;            │       │        │
;;  q ───────►│   │   │   ─────┼──────────────────────────────────────┐
;;            │   │   │        │                                      │
;;            └───┼───┴────────┘                                      │
;;                │                                                   │
;;                │ front-ptr                                         │ rear-ptr
;;                │                                                   │
;;                ▼                                                   ▼
;;            ┌───────┬────────┐        ┌───────┬────────┐        ┌───────┬────────┐
;;            │       │        │        │       │        │        │       │        │
;;            │   │   │   ─────┼───────►│   │   │   ─────┼───────►│   │   │        │
;;            │   │   │        │        │   │   │        │        │   │   │        │
;;            └───┼───┴────────┘        └───┼───┴────────┘        └───┼───┴────────┘
;;                │                         │                         │
;;                │                         │                         │
;;                │                         │                         │
;;                ▼                         ▼                         ▼
;;            ┌───────┐                 ┌───────┐                 ┌───────┐
;;            │       │                 │       │                 │       │
;;            │   a   │                 │   b   │                 │   c   │
;;            │       │                 │       │                 │       │
;;            └───────┘                 └───────┘                 └───────┘

;; 维护此数据结构
(define (front-ptr queue)
  (car queue))
(define (rear-ptr queue)
  (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

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
       queue]
      [else
       (set-cdr! (rear-ptr queue) new-pair)
       (set-rear-ptr! queue new-pair)
       queue])))

;; 出队：将 front-ptr 指向 front-ptr 指向的 pair 的 cdr
(define (delete-queue! queue)
  (cond
    [(empty-queue? queue)
     (error "DELETE! called with an empty queue" queue)]
    [else
     (set-front-ptr! queue (cdr (front-ptr queue)))
     queue]))

(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)
(delete-queue! q)
(insert-queue! q 'c)
(insert-queue! q 'd)
(delete-queue! q)

;; {a, *}->{b, *}->{c, *}->{d, ()}
