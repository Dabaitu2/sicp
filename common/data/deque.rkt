#lang sicp

;; 双端队列 double-end queue
;; 每一个节点 不再是一个简单的 cons, 而是一个指向两边的 list
;; 这样我们才可以找到 rear 的前一个

(define (make-deque)
  (let ([front-ptr '()] [rear-ptr '()])

    (define (display-deque)
      (define (iter ptr)
        (cond
          [(or (null? ptr) (null? (cdr ptr))) (display "~")]
          [else
           (display (cadr ptr))
           (display " ")
           (iter (cddr ptr))]))
      (iter front-ptr))

    (define (empty-deque?)
      (or (null? front-ptr) (null? rear-ptr)))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    ;; selectors
    (define (front-deque)
      (if (empty-deque?)
          (error "FRONT called with an empty deque")
          (cadr front-ptr)))

    (define (rear-deque)
      (if (empty-deque?)
          (error "REAR called with an empty deque")
          (cadr rear-ptr)))

    ;; mutators
    (define (front-insert-queue! item)
      (let ([new-pair (list '() item '())])
        (cond
          [(empty-deque?)
           (set-front-ptr! new-pair)
           (set-rear-ptr! new-pair)]
          [else
           ;; 插入的时候要维护双端节点
           (set-cdr! (cdr new-pair) front-ptr)
           (set-car! front-ptr new-pair)
           (set-front-ptr! new-pair)])))

    (define (rear-insert-queue! item)
      (let ([new-pair (list '() item '())])
        (cond
          [(empty-deque?)
           (set-front-ptr! new-pair)
           (set-rear-ptr! new-pair)]
          [else
           (set-cdr! (cdr rear-ptr) new-pair)
           (set-car! new-pair rear-ptr)
           (set-rear-ptr! new-pair)])))

    (define (front-delete-deque!)
      (cond
        [(empty-deque?)
         (error "DELETE! called with an empty queue")]
        [else
         ;; 注意这里不要纠结在 list 的实现上了
         ;; 我们处理的就只是一个个的 cons 就好了
         ;; 因此取得第一个元素就是 (car ptr)
         ;; 第二个就是 (cadr ptr)
         ;; 第三个就是 (cddr ptr)
         (set-front-ptr! (cddr front-ptr))
         (if (empty-deque?)
             (set-rear-ptr! '())
             (set-car! front-ptr '()))]))

    (define (rear-delete-deque!)
      (cond
        [(empty-deque?)
         (error "DELETE! called with an empty queue")]
        [else
         (set-rear-ptr! (car rear-ptr))
         (if (empty-deque?)
             (set-front-ptr! '())
             (set-cdr! (cdr rear-ptr) '()))]))

    (define (dispatch m)
      (cond
        [(eq? m 'empty-deque?) (empty-deque?)]
        [(eq? m 'front-deque) (front-deque)]
        [(eq? m 'rear-deque) (rear-deque)]
        [(eq? m 'front-insert-deque!) front-insert-queue!]
        [(eq? m 'rear-insert-deque!) rear-insert-queue!]
        [(eq? m 'front-delete-deque!) (front-delete-deque!)]
        [(eq? m 'rear-delete-deque!) (rear-delete-deque!)]
        [(eq? m 'display-deque) (display-deque)]
        [else (error "Invalid operation: " m)]))
    dispatch))

(#%provide make-deque)
