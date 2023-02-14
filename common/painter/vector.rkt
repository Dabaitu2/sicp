#lang racket

;; 构造向量
;; 整个 Painter 库都在遵循一个抽象数据的基本法则
;; constructor + selector
;; 以及基于其上建立层层抽象屏障，从最底层的向量, 到构建 Segment，Frame, Painter，无一不在这样使用
;; 
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect vec1 vec2)
  (cons (+ (xcor-vect vec1) (xcor-vect vec2))
        (+ (ycor-vect vec1) (ycor-vect vec2))))

(define (sub-vect vec1 vec2)
  (cons (- (xcor-vect vec1) (xcor-vect vec2))
        (- (ycor-vect vec1) (ycor-vect vec2))))

(define (scale-vect scale vec)
  (cons (* scale (xcor-vect vec))
        (* scale (ycor-vect vec))))

(#%provide make-vect
           xcor-vect
           ycor-vect
           add-vect
           sub-vect
           scale-vect)
