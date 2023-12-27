#lang sicp

(#%require "../../../common/data/stream.rkt")

;; 实现一个 merge-weighted 函数，它除了接受 s1, s2 还提供一个用于判断权重的函数
;; 用于实现对 s1, s2 合并时的元素先后顺序排序
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

(define pair1
  (weighted-pairs integers
                  integers
                  (lambda (pair) (apply + pair))))

(define (nondivisible x)
  (not (or (= 0 (remainder x 2))
           (= 0 (remainder x 3))
           (= 0 (remainder x 5)))))

(define nondivisible-stream
  (stream-filter nondivisible integers))

(define pair2
  (weighted-pairs nondivisible-stream
                  nondivisible-stream
                  (lambda (pair)
                    (let ([i (car pair)] [j (cadr pair)])
                      (+ (* 2 i) (* 3 j) (* 5 i j))))))

(stream-refs 10 pair1)
(stream-refs 10 pair2)
