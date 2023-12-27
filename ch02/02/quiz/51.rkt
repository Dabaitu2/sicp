
#lang sicp

(#%require "../04-painter/frame.rkt")
(#%require "../04-painter/vector.rkt")
(#%require "../04-painter/transform.rkt")

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ([m (frame-coord-map frame)])
      (let ([new-origin (m origin)])
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))


(define (below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ([paint-top
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)]
          [paint-bottom
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 1.0 0.0))])
      (lambda (frame)
        (paint-top frame) (paint-bottom frame)))))

(define (below2 painter1 painter2)
  (rotate90 (beside painter1 painter2)))
