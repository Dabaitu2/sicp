#lang racket
;; 如何实现 Delay 和 force ？
;; 实际上方案是相当朴素的, 就是 currying 的思想
;; (define (delay exp)
;;   (lambda () exp))
;;
;; force 就是针对这个对象再求一次值就可以了
(define (force delayed-obj)
  (delayed-obj))

;; 我们针对 delay 再增加一层缓存优化
(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (delay exp)
  (memo-proc (lambda () exp)))

(define (cons-stream x y)
  (cons x (delay y)))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define stream-null? null?)
(define the-empty-stream '())

;; (define (stream-map proc s)
;;   (if (stream-null? s)
;;       the-empty-stream
;;       (cons-stream (proc stream-car s)
;;                    (stream-map proc (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       ;; 处理每一个 stream 的第一个数据，并求值
       (apply proc (map stream-car argstreams))
       ;; cons-stream 会使得这里的数据先不被求值，因为包裹在 delay 里面了
       ;; 如果需要调用时，这里会被求值，此时
       ;; 产出每一个 stream 的 cdr 数据,
       ;; 求值获得下一个 stream, 然后交给 stream map 做进一轮调用
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream))))
  )

;; (define (get-prime-of-interval a b)
;;   (stream-car (stream-cdr (stream-filter prime? (stream-enumerate-interval a b)))))
;;
;; (get-prime-of-interval 10000 100000)


(#%provide cons-stream stream-cdr stream-car stream-null? the-empty-stream stream-enumerate-interval display-line stream-map stream-ref display-stream stream-filter)

