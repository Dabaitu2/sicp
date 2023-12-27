#lang sicp

(#%require "../../../common/data/stream.rkt")

;; 修改 pairs 使其可以产生所有整数序对
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   ;; 这里就交换顺序，让递归的元素变成 s2， 从而让 s1 和 s2 交错
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave (interleave (stream-map (lambda (x)
                                         (list (stream-car s) x))
                                       (stream-cdr t))
                           ;; 在处理完右上方后,下次再处理下左下方
                           (stream-map (lambda (x)
                                         (list x (stream-car t)))
                                       (stream-cdr s)))
               (pairs (stream-cdr s) (stream-cdr t)))))

(stream-refs 30 (pairs integers integers))

