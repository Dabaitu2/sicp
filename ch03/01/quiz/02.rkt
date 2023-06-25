#lang racket

;; 实现 监控打点过程 (hook) 来统计过程的调用次数，此函数接受一个过程,
;; 它以一个过程 f 作为输入，该过程本身有一个输入 。make-monitored返回的结果是第三个过程，比奶说mf，它将用一个内部计数器维持着自己被调用的次数。
;; 如果mf 的输入是特殊符号 how-many-calls?，
;; 那么mf 就返回内部计数器的值，
;; 如果输入是特殊符号 reset-count，
;; 那么 mf 就将计数器重新设置为 0，
;; 对于任何其他输入，mf 將返回过程 f 应用于这一输入的结果，并將内部计数器加 1

(define (make-monitored f)
  (let ([invoke-count 0])
    (define (how-many-calls?)
      invoke-count)
    (define (reset-count)
      (set! invoke-count 0))
    (lambda (signal)
      (cond
        [(eq? 'how-many-calls? signal) (how-many-calls?)]
        [(eq? 'reset-count signal) (reset-count)]
        [else
         (begin
           (set! invoke-count (+ invoke-count 1))
           (f signal))]))))

(define s (make-monitored sqrt))
(s 2)
(s 3)
(s 4)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
