#lang sicp

(#%require "../../../common/data/stream.rkt")
(#%require "../../../common/math/num-theory.rkt")

;; 求 Ramanujan 数形成的流
;; 可以以多于一种方式表达为两个立方数之和的数被称为 Ramanujan 数
;; 我们可以通过创造一个 i^3 + j^3 作为权重的流来获得可以表示为写为两个立方数之和的数
;; 同时通过 weighted-pairs 将这些数按从小到大进行排列
;; 然后在流中找到具有同样权重的前后两个相邻排列的 pair, 这样就可以获得对应的 ramanujan 排列
(define cube-stream (stream-map cube integers))
(define sum-of-cube
  (weighted-pairs cube-stream
                  cube-stream
                  (lambda (pair)
                    (+ (car pair) (cadr pair)))))

(define ramanujan
  (stream-map
   ;; 3. 计算结果和
   (lambda (x)
     (let ([first (car x)] [second (cadr x)])
       (list (+ (car first) (cadr first)) first second)))
   ;; 2. 获取前后相同的那部分
   (stream-filter (lambda (x)
                    (let ([first (car x)] [second (cadr x)])
                      (= (+ (car first) (cadr first))
                         (+ (car second) (cadr second)))))
                  ;; 1. 将流和平移后的流结合
                  (stream-map (lambda (x y) (list x y))
                              sum-of-cube
                              (stream-cdr sum-of-cube)))))

(stream-refs 10 ramanujan)
