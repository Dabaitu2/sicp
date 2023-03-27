#lang racket

(#%require "./vector.rkt")

;; 实现 Frame
;; origin vector specify the offset of the frame's origin from some absolute origin in the plane (translate X/Y)
;; edge vectors specify the offsets of the fframe's corner from its origin. (rotateZ + scaleX/Y)
;;
;; 在这里，我们依旧使用抽象屏障，首先定义
;; 一个constructor make-frame
;; 他利用 三个向量(1 origin vector + 2 edge vectors) 产生一个 Frame
;; originVector 代表了原点位置
;; edgeVector   代表了缩放大小
;; 需要定义 selector: origin-frame edge1-frame edge2-frame

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

;; 接下来: 我们使用单位正方形的坐标 (0 ≤ x, y ≤ 1) 去描述图像
;; 也就是说我们只是只在 0-1 范围内去指定一些点，线，面，而这个 0-1 的单位正方形会被映射到 frame 中
;;
;; 对于每个 Frame 我们要为他关联一个 Frame Coordinate map 来实现这个事情
;; 借助其完成图像的位移和伸缩
;;
;; 这个映射会将单位正方形变换到对应的 frame 中
;; 针对单位正方形中的任意一个点，我们都以向量 v = (x, y) 表示，那么这样的映射关系便为这个公式
;; Origin(Frame) + x * Edge1(Frame) + y * Edge2(Frame)
;;
;; x * Edge1(Frame) + y * Edge2(Frame) 的几何意义
;;
;; 单位正方形中的点 (x, y) ,  将其看作一个向量 v1
;; v1 放到 Edge1， Edge2 这两条边形成的矩形中，假设映射结果为
;;
;; 向量 v2 (newX, newY) = k1 * Edge1 + k2 * Edge2 (平行四边形法则)
;;
;; 我们要求出这个 k1 和 k2,
;; 我们又知道 v1 存在于单位正方形中，它的 x,y 刚好就是代表它占 1 这个单位的比例
;; 那么就可以直接代换到 k1 k2, 也就是 v2 = x * Edge1(frame) + y * Edge2(frame) = (newX, newY)
;;
;; Origin(Frame) + v2 对应的就是这个点相对于原点需要在 origin-frame 的 x，y 方向进行平移， 最终获得了这样的映射关系
;;
;; 在这样的映射下，在单位正方形坐标中
;; (0, 0) 被映射到 frame 的原点 Origin(Frame) + 0 * edge1(frame) + 0 * edge2(frame) = origin(frame)
;; (1，1) 被映射到与原点对角的那个点，
;; (0.5, 0.5) 被映射到给定框架的中心点。
;;
;; 我们可以通过下面过程建立起框架的坐标映射
;; xcor-vect 是获得 v 向量的 x
;; ycor-vect 是获得 v 向量的 y
;; scale-vect 将 edge Vector 进行放缩
;; add-vect 对向量进行加法运算
;; 所以这个函数其实就是 上面的向量表达式的代码表示
;; 这个过程的结果依然是一个过程
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(#%provide make-frame
           origin-frame
           edge1-frame
           edge2-frame
           frame-coord-map)
