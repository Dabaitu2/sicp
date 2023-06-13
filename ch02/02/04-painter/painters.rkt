#lang racket

;; 如果要使用一些原生的东西，就用 rackt lang 吧，不然 sicp 的 list / cons 和 racket 的不兼
;; 因为 sicp 的是我们自己实现的语法特性 (提前赞叹一下)
(#%require racket/gui/base)
(#%require "../../../common/data/conventional-interface.rkt")
(#%require "./frame.rkt")
(#%require "./segment.rkt")
(#%require "./vector.rkt")

;; helpers
(define picture-size 300)
(define target
  (make-bitmap picture-size picture-size))
(define bitmap-dc
  (new bitmap-dc% [bitmap target]))
;; 这里的 dc 是写死的, 实际上可以取出
(define (draw-line start end)
  (send bitmap-dc
        draw-line
        (xcor-vect start)
        (ycor-vect start)
        (xcor-vect end)
        (ycor-vect end)))


;; 实现 Painter
;; Painter 其实是一个绘图过程, 可以理解为我们在 canvas 上面
;; 调用了一大堆命令所做的事情
;; 而 frame 其实类似于 canvas 画布本身, 我们建立画布就是要指定一个相对于屏幕
;; 坐标系原点的位置，然后指定 Frame 以便于我们进行映射
;;
;; 而 painter 本身是基于一个 单位正方形绘图，被放缩到这个画布上的
;;
;; 同样，还是使用抽象屏障
;; 我们在这里实现一个画折线的画家
;;
;; segment-list 是单位正方形上的线段序列
;; segments->painter 将其转化画到 frame 上
;;
;; draw-line 接受 frame 上的两个点，在 frame 上画出一条直线
;; start-segment 获得单位正方形线段的起点
;; end-segment 获得单位正方形线段的终点
;;
;; 通过将 Painter 抽象成为过程，我们进一步确立了抽象屏障
;; 所有的 Painter 只要能够接受一个 frame 就能画出一些东西
;; 那么我们可以组合多个基础 Painter 去搞出一个复杂 Painter
;; 例如，我们可以有一个 Point Painter
;;
;; 它只负责将一个单位正方形上的点映射到 frame 上，并且涂黑它
;; 基于这个 Painter，我们就能实现画一根线的 Painter
;; 它只需要一个一个找到起点和终点之间的点，一个一个调用 Point Painter 就可以画出一条线
;; 从代码中可以看出，Painter 对 frame 的实现是不清楚的，这就是抽象屏障
(define (segments->painter segment-list)
  (display (start-segment (car segment-list)))
  (lambda (frame)
    (for-each (lambda (segment)
                (draw-line ((frame-coord-map frame)
                            (start-segment segment))
                           ((frame-coord-map frame)
                            (end-segment segment))))
              segment-list)))

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

(define diamond
  (segments->painter
   (list (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))
         (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5))
         (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))
         (make-segment (make-vect 0.5 0.0) (make-vect 0.0 0.5)))))

(define wave
  (segments->painter (list
                      (make-segment (make-vect .25 0) (make-vect .35 .5))
                      (make-segment (make-vect .35 .5) (make-vect .3 .6))
                      (make-segment (make-vect .3 .6) (make-vect .15 .4))
                      (make-segment (make-vect .15 .4) (make-vect 0 .65))
                      (make-segment (make-vect 0 .65) (make-vect 0 .85))
                      (make-segment (make-vect 0 .85) (make-vect .15 .6))
                      (make-segment (make-vect .15 .6) (make-vect .3 .65))
                      (make-segment (make-vect .3 .65) (make-vect .4 .65))
                      (make-segment (make-vect .4 .65) (make-vect .35 .85))
                      (make-segment (make-vect .35 .85) (make-vect .4 1))
                      (make-segment (make-vect .4 1) (make-vect .6 1))
                      (make-segment (make-vect .6 1) (make-vect .65 .85))
                      (make-segment (make-vect .65 .85) (make-vect .6 .65))
                      (make-segment (make-vect .6 .65) (make-vect .75 .65))
                      (make-segment (make-vect .75 .65) (make-vect 1 .35))
                      (make-segment (make-vect 1 .35) (make-vect 1 .15))
                      (make-segment (make-vect 1 .15) (make-vect .6 .45))
                      (make-segment (make-vect .6 .45) (make-vect .75 0))
                      (make-segment (make-vect .75 0) (make-vect .6 0))
                      (make-segment (make-vect .6 0) (make-vect .5 .3))
                      (make-segment (make-vect .5 .3) (make-vect .4 0))
                      (make-segment (make-vect .4 0) (make-vect .25 0))
                      )))

(make-object image-snip% target)
;; (wave frame)

(#%provide wave
           diamond
           frame-cross
           frame-outline
           segments->painter)
