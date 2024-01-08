#lang sicp

(#%require "../../../common/data/stream.rkt")

(define random-init 987654321)
(define a 1103515245)
(define c 12345)
(define m (expt 2 31))

(define (rand-update x)
  (remainder (+ (* a x) c) m))

;; 状态化的随机数生成器
(define rand
  (let ([x random-init])
    (lambda (m)
      (cond
        [(eq? m 'generate)
         (set! x (rand-update x))
         x]
        [(eq? m 'reset)
         (lambda (new-init) (set! x new-init))]
        [else (error "Unknown request -- RAND " m)]))))


(define requests
  (list->stream '((reset 0) (generate)
                            (generate)
                            (generate)
                            (reset 0)
                            (generate)
                            (generate)
                            (reset 0)
                            (generate))))

;; 流式的随机数生成器, 接受一个流式的命令, 每次调用返回一个随机数
;; 如果传入的是 reset，则重置随机数生成器
(define (rand-stream requests)
  (define (rand-stream-helper requests last-init)
    (let ([req (stream-car requests)])
      (let ([m (car req)])
        (cond
          [(eq? m 'generate)
           (let ([new-init (rand-update last-init)])
             (cons-stream new-init
                          (rand-stream-helper
                           (stream-cdr requests)
                           new-init)))]
          [(eq? m 'reset)
           (let ([new-init (rand-update (cadr req))])
             (cons-stream new-init
                          (rand-stream-helper
                           (stream-cdr requests)
                           new-init)))]
          [else
           (error "Unknown opearaion: RAND-STREAM")]))))
  (rand-stream-helper requests random-init))

(rand 'generate)
(rand 'generate)
((rand 'reset) 0)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

(stream-refs 9 requests)
(stream-refs 9 (rand-stream requests))
