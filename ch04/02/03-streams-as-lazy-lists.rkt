#lang sicp

;; 基于 1. 2 小节所给出的 惰性求值 的 evaluator 实现
;; 我们甚至不再需要显式的使用特殊形式 cons-stream 或 delay 去构造无穷流
;; 这两者作为特殊形式本身就有一些缺陷：
;; 1. 无法和高阶过程一起使用
;; 2. 流必须创建为一类特殊的对象，并且重新实现 序列相关的诸多接口

;; 那么我们可以用什么策略来替换呢
;; 1. 我们可以改变 cons 的定义，扩充惰性求值器，将 cons 变为一种 non-strict 的操作
;; 2. 我们可以不把 cons 实现为基本过程(primitive), 而是通过一般过程去实现

;; 牛逼的代码, 需要在我们 解释器环境中使用
;; 不要直接运行，不然会死循环，想在一般环境使用需要用 cons-stream 特殊形式
(define (cons x y)
  (lambda (m) (m x y)))

;; z 这个 cons 实际上是个函数， 而 (lambda (p q) p) 作为 实际的 m 被提供给 cons 就可以获得闭包缓存的值
(define (car z)
  (z (lambda (p q) p)))

;; 同理，不多赘述
(define (cdr z)
  (z (lambda (p q) q)))

;; 这样使用一般过程去实现 cons ，我们就可以将 cons 的参数变成惰性的了
;; 因为它就是个一般过程，一般过程的参数默认都是 delay 的

;; 同样借助这样的过程, 我们原先实现的 list, 就兼具了有穷数据和无穷流的两种能力
(define (list-ref items n)
  (if (= n 0) (car items) (list-ref (cdr items) (- n 1))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(define (add-lists list1 list2)
  (cond
    [(null? list1) list2]
    [(null? list2) list1]
    [else
     (cons (+ (car list1) (car list2))
           (add-lists (cdr list1) (cdr list2)))]))

(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))

;; 基于完全延时的 cons 过程，我们求 3.5.4 节的积分时也不再需要手动写一个 delay 特殊形式了
;;  同时，我们也不需要单独的去使用 stream 相关的惰性求值 api 去将它和 list 分开了
(define (integral integrand initial-value dt)
  (define int
    (cons initial-value
          (add-lists (scale-list integrand dt) int)))
  int)

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y)
