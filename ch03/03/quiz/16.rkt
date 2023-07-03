#lang sicp

;; we skip 3.15 becasue I'm really not good at painting

;; the wrong way to count the number of pairs in any list structure
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x)) (count-pairs (cdr x)) 1)))

(define x    (cons 'foo '()))
(define y    (cons x x))
(define a (cons y '()))

;; the situation that return 4
;; 虽然，看起来实际上确实是有 4 个 cons, 但这其中 y = (cons x x) 中的两个 x 实际上是 "同一" 个
;; 而他们被 y 的 car 和 cdr 共享了, 所以本质上只有三个 cons !
;; 从盒子指针便可以看出
;;    a -> ( . ) -> null 
;;          | 
;;          v 
;;    y -> ( . ) 
;;          | | 
;;          v v 
;;    x -> ( . ) -> 'null 
;;          | 
;;          v 
;;         'foo
(count-pairs a)


;; the situation that return 7
;; x * 4 + y * 2 + b * 1 = 7
(define b (cons y y))
(count-pairs b)
