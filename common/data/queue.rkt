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

;; 用包含局部状态 & 消息传递的过程来表示队列

(define (make-queue)
  (let ([front-ptr '()] [rear-ptr '()])
    (define (empty-queue?)
      (null? front-ptr))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (insert-queue! item)
      (let ([new-pair (cons item '())])
        (cond
          [(empty-queue?)
           (set-front-ptr! new-pair)
           (set-rear-ptr! new-pair)
           front-ptr]
          [else
           (set-cdr! rear-ptr new-pair)
           (set-rear-ptr! new-pair)
           front-ptr])))

    (define (delete-queue!)
      (cond
        [(empty-queue?)
         (error "DELETE! called with an empty queue")]
        [else
         (set-front-ptr! (cdr front-ptr))
         front-ptr]))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))

    (define (dispatch m)
      (cond
        [(eq? m 'empty-queue?) (empty-queue?)]
        [(eq? m 'front-queue) (front-queue)]
        [(eq? m 'insert-queue!) insert-queue!]
        [(eq? m 'delete-queue!) (delete-queue!)]
        [else (error "Invalid operation: " m)]))
    dispatch))

(#%provide make-queue)
