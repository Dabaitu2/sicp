#lang sicp

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

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
