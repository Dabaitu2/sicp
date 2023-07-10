#lang sicp

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

(define q1 (make-queue))
(display ((q1 'insert-queue!) 'a))
(newline)
(display ((q1 'insert-queue!) 'b))
(newline)
(display (q1 'front-queue))
(newline)
(display (q1 'delete-queue!))
