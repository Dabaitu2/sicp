#lang sicp
(#%require "../../../common/data/stream.rkt")

;; helpers
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   ;; 这里就交换顺序，让递归的元素变成 s2， 从而让 s1 和 s2 交错
                   (interleave s2 (stream-cdr s1)))))

;; pairs 可以产生二元组的流
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave (stream-map (lambda (x)
                             (list (stream-car s) x))
                           (stream-cdr t))
               (pairs (stream-cdr s) (stream-cdr t)))))

;; 以三个无穷流为参数，生成三元组 (Si, Tj, Uk) 的流，其中要求 i <= j <= k
;; 由于 pair 已经可以确定产生所有 i <= j  的二元组，我们只需要将这个二元组流看作另一个流
;; 再和新的一个流进行合并就可以了, 其中将新的流放到最前面 pair
;; 抽象的能力可以使得我们从细节中解放
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave (stream-map (lambda (x)
                             ;; 这里需要 cons，因为 x 本身是一个 list，再 list
                             ;; 的话就套娃了
                             (cons (stream-car s) x))
                           (stream-cdr (pairs t u)))
               (triples (stream-cdr s)
                        (stream-cdr t)
                        (stream-cdr u)))))

(stream-refs 10 (triples integers integers integers))

(define (square x)
  (* x x))
(define phythagorean-numbers
  (stream-filter (lambda (x)
                   (= (+ (square (car x)) (square (cadr x)))
                      (square (caddr x))))
                 (triples integers integers integers)))

;; (stream-refs 10 (phythagorean-numbers))

;; 这个流求解的速度还是比较慢的..因为需要大量的回溯计算
(stream-ref phythagorean-numbers 0)
(stream-ref phythagorean-numbers 1)
(stream-ref phythagorean-numbers 2)
(stream-ref phythagorean-numbers 3)
(stream-ref phythagorean-numbers 4)
(stream-ref phythagorean-numbers 5)
(stream-ref phythagorean-numbers 6)
;; (stream-refs 5 phythagorean-numbers)
;; (stream-car (phythagorean-numbers))
