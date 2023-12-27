#lang sicp

(#%require "../../../common/data/stream.rkt")
(#%require "../../../common/math/num-theory.rkt")

;; 获得可以以三种方式表达为两个平方数之和的流
(define square-stream (stream-map square integers))

;; 平方数之和形成的流结果按照从小到大的排列
(define sum-of-square
  (weighted-pairs square-stream
                  square-stream
                  (lambda (pair)
                    (+ (car pair) (cadr pair)))))

(define wanted
  (stream-map
   ;; 3. 计算结果和
   (lambda (x)
     (let ([first (car x)]
           [second (cadr x)]
           [thrid (caddr x)])
       (list (+ (car first) (cadr first))
             first
             second
             thrid)))
   ;; 2. 获取前后三个均相同的那部分
   (stream-filter
    (lambda (x)
      (let ([first (car x)]
            [second (cadr x)]
            [thrid (caddr x)])
        (= (+ (car first) (cadr first))
           (+ (car second) (cadr second))
           (+ (car thrid) (cadr thrid)))))
    ;; 1. 将流和平移后的流结合
    (stream-map (lambda (x y z) (list x y z))
                sum-of-square
                (stream-cdr sum-of-square)
                (stream-cdr (stream-cdr sum-of-square))))))

;; actually we can combine the result again and filter out those that have both same elements
(stream-refs 10 wanted)
