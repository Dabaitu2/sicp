#lang sicp

(#%require "./frame.rkt")
(#%require "./vector.rkt")

;; 对于 Painter (所使用的 Frame) 进行转换, 生成一个新 painter
;; 实际上只是将 Painter 的入参 frame 先做一番转换再调用 Painter
;; corner 是新 frame 的两个 edge 的终点
;; 我们操纵变换时，不需要考虑 frame 定义的实际坐标位置，而是通过
;; 在单位正方形中指定对应坐标，由 frame-coord-map 结合原始 frame
;; 帮助我们进行实际的映射
;; 最后获得真正的 frame
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ([m (frame-coord-map frame)])
      (let ([new-origin (m origin)])
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

;; 基于通用方法再做一些抽象
;; 上下翻转
;; (0, 0) -> (0, 1)
;; (0, 1) -> (0, 0)
;; (0, 1) -> (1, 0)
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)))

;; 收缩到右上角
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


;; 逆时针旋转 90 度
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;; 逆时针旋转 180 度
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

;; 逆时针旋转 270 度
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; 向中心收缩
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


;; 基于抽象进行组合
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ([paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0))]
          [paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))])
      (lambda (frame)
        (paint-left frame) (paint-right frame)))))

(define (below painter1 painter2)
  (rotate90 (beside painter1 painter2)))


(#%provide transform-painter
           flip-vert
           flip-horiz
           shrink-to-upper-right
           rotate90
           rotate180
           rotate270
           squash-inwards
           beside)
