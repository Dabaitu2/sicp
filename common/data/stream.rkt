#lang sicp
;; 如何实现 Delay 和 force ？
;; 实际上方案是相当朴素的, 就是 currying 的思想
;; (define (delay exp)
;;   (lambda () exp))
;;
;; force 就是针对这个对象再求一次值就可以了
;; (define (force delayed-obj)
;;   (delayed-obj))

;; 我们针对 delay 再增加一层缓存优化
;; (define (memo-proc proc)
;;   (let ((already-run? false)
;;         (result false))
;;     (lambda ()
;;       (if (not already-run?)
;;           (begin (set! result (proc))
;;                  (set! already-run? true)
;;                  result)
;;           result))))

;; (define (delay exp)
;;   (memo-proc (lambda () exp)))

;; 这里的 cons-stream 实际上是用不了的，因为按照求值模型，y 还是会在调用 cons-stream 的时候被求值
;; 在这里我们只能先跳过, 使用 sicp 提供的
;; (define (cons-stream x y)
;;   (cons x (delay y)))

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

;; 如果参数是多个流, 产出也只是一个，有点类似于 merge?
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
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1)
                                              high))))

(define (stream-filter pred stream)
  (cond
    [(stream-null? stream) the-empty-stream]
    [(pred (stream-car stream))
     (cons-stream (stream-car stream)
                  (stream-filter pred (stream-cdr stream)))]
    [else (stream-filter pred (stream-cdr stream))]))

(define (add-stream s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (merge s1 s2)
  (cond
    [(stream-null? s1) s2]
    [(stream-null? s2) s1]
    [else
     (let ([s1car (stream-car s1)] [s2car (stream-car s2)])
       (cond
         [(< s1car s2car)
          (cons-stream s1car (merge (stream-cdr s1) s2))]
         [(> s1car s2car)
          (cons-stream s2car (merge s1 (stream-cdr s2)))]
         [else
          (cons-stream s1car
                       (merge (stream-cdr s1)
                              (stream-cdr s2)))]))]))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (stream-refs count stream)
  (map (lambda (s) (stream-ref stream s))
       (enumerate-interval 0 (- count 1))))

;; 生成 0 - n 的列表

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-stream ones integers)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;; 这样就可以实现出我们所需要的流了
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave (stream-map (lambda (x)
                             (list (stream-car s) x))
                           (stream-cdr t))
               (pairs (stream-cdr s) (stream-cdr t)))))

(define (merge-weighted s1 s2 weight)
  (cond
    [(stream-null? s1) s2]
    [(stream-null? s2) s1]
    [else
     (let ([s1car (stream-car s1)] [s2car (stream-car s2)])
       (cond
         [(<= (weight s1car) (weight s2car))
          (cons-stream
           s1car
           (merge-weighted (stream-cdr s1) s2 weight))]
         [else
          (cons-stream
           s2car
           (merge-weighted s1 (stream-cdr s2) weight))]))]))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-stream (scale-stream integrand dt)
                             int)))
  int)

(#%provide cons-stream
           stream-cdr
           stream-car
           stream-null?
           the-empty-stream
           stream-enumerate-interval
           display-line
           stream-map
           stream-ref
           display-stream
           stream-filter
           add-stream
           mul-streams
           scale-stream
           integers
           ones
           merge
           stream-refs
           pairs
           weighted-pairs
           merge-weighted
           integral)
