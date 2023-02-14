#lang racket
(require racket/gui)

; 2.2.4 Example: A Picture Language
(display "2.2.4 Example: A Picture Language")
(newline)

; First up is some code that is required to make the rest of this section work. Skip over it and
; follow along with the text if you like. I found it much easier to follow along once the examples
; started working.

; Vector
(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))
(define (add-vect vec1 vec2)
  (make-vect (+ (xcor-vect vec1) (xcor-vect vec2))
             (+ (ycor-vect vec1) (ycor-vect vec2))))
(define (sub-vect vec1 vec2)
  (make-vect (- (xcor-vect vec1) (xcor-vect vec2))
             (- (ycor-vect vec1) (ycor-vect vec2))))
(define (scale-vect scale vect)
  (make-vect (* scale (xcor-vect vect))
             (* scale (ycor-vect vect))))

; Segment
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

; Frame
(define (make-frame origin edge1 edge2 dc)
  (list origin edge1 edge2 dc))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))
(define (dc frame)
  (cadddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect
      (scale-vect (xcor-vect v) (edge1-frame frame))
      (scale-vect (ycor-vect v) (edge2-frame frame))))))

; Painter Generators

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
                (let ([start ((frame-coord-map frame)
                              (start-segment segment))]
                      [end ((frame-coord-map frame)
                            (end-segment segment))])
                  (send (dc frame)
                        draw-line
                        (xcor-vect start)
                        (ycor-vect start)
                        (xcor-vect end)
                        (ycor-vect end))))
              segment-list)))

(define (segments->text segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ([start ((frame-coord-map frame)
                     (start-segment segment))]
             [end ((frame-coord-map frame)
                   (end-segment segment))])
         (fprintf (current-output-port)
                  "Draw Line:StartX:~a Y:~a EndX:~a Y:~a\n"
                  (xcor-vect start)
                  (ycor-vect start)
                  (xcor-vect end)
                  (ycor-vect end))))
     segment-list)))

; Painters
(define (frame-outline frame)
  ((segments->painter
    (list (make-segment (make-vect 0 0) (make-vect 1 0))
          (make-segment (make-vect 1 0) (make-vect 1 1))
          (make-segment (make-vect 1 1) (make-vect 0 1))
          (make-segment (make-vect 0 1) (make-vect 0 0))))
   frame))

(define (frame-cross frame)
  ((segments->painter
    (list (make-segment (make-vect 0 0) (make-vect 1 1))
          (make-segment (make-vect 1 0) (make-vect 0 1))))
   frame))

(define (frame-diamond frame)
  ((segments->painter
    (list
     (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
     (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
     (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
     (make-segment (make-vect 0.5 0) (make-vect 0 0.5))))
   frame))

(define (wave frame)
  ((segments->painter
    (list
     (make-segment (make-vect 0.165 0.945)
                   (make-vect 0.465 0.665)) ; Left Leg
     (make-segment (make-vect 0.465 0.665)
                   (make-vect 0.465 0.285)) ; Body
     (make-segment (make-vect 0.465 0.455)
                   (make-vect 0.745 0.585)) ; Right arm
     (make-segment (make-vect 0.465 0.665)
                   (make-vect 0.755 0.925)) ;
     (make-segment (make-vect 0.475 0.455)
                   (make-vect 0.185 0.615)) ; ?
     (make-segment (make-vect 0.245 0.265)
                   (make-vect 0.685 0.295)) ; Head - Bot
     (make-segment (make-vect 0.685 0.295)
                   (make-vect 0.685 0.035)) ;
     (make-segment (make-vect 0.685 0.035)
                   (make-vect 0.245 0.065)) ;
     (make-segment (make-vect 0.245 0.065)
                   (make-vect 0.245 0.265))))
   frame))

; Transformer Generator

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ([m (frame-coord-map frame)])
      (let ([new-origin (m origin)])
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)
                  (dc frame)))))))

; Transformers
(define (flip-vert painter)
  (transform-painter
   painter
   (make-vect 0.0 1.0) ; new origin
   (make-vect 1.0 1.0) ; new end of edge1
   (make-vect 0.0 0.0))) ; new end of edge2

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.35 0.35)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (beside painter1 painter2)
  (let ([split-point (make-vect 0.5 0.0)])
    (let ([paint-left (transform-painter
                       painter1
                       (make-vect 0.0 0.0)
                       split-point
                       (make-vect 0.0 1.0))]
          [paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))])
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ([split-point (make-vect 0 0.5)])
    (let ([paint-top (transform-painter painter1
                                        (make-vect 0.0 0.0)
                                        (make-vect 1.0 0.0)
                                        split-point)]
          [paint-bot (transform-painter
                      painter2
                      split-point
                      (make-vect 1.0 0.5)
                      (make-vect 0.0 1.0))])
      (lambda (frame)
        (paint-top frame)
        (paint-bot frame)))))

(define (flipped-pairs painter)
  (let ([painter2 (beside painter (flip-vert painter))])
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (right-split painter (- n 1))])
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (up-split painter (- n 1))])
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ([up (up-split painter (- n 1))]
            [right (right-split painter (- n 1))])
        (let ([top-left (beside up up)]
              [bottom-right (below right right)]
              [corner (corner-split painter (- n 1))])
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; (display "Wave") (newline)
(define target-1 (make-bitmap 300 300))
(define DC-1 (new bitmap-dc% [bitmap target-1]))
(define test-frame
  (make-frame (make-vect 0 0)
              (make-vect 300.0 0.0)
              (make-vect 0.0 300.0)
              DC-1))
(make-object image-snip% target-1)
(wave test-frame)

(#%provide wave
           beside
           flip-vert
           below
           test-frame
           flip-horiz
           rotate180
           add-vect
           origin-frame
           scale-vect
           xcor-vect
           ycor-vect
           edge1-frame
           edge2-frame)
