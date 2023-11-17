#lang sicp
(#%require "../../../common/math/num-theory.rkt")


;; 流的目标是：自动和透明的将流的构造过程和使用过程 交织(interleave) 在一起
;; 也就是，当流创建数据的时候，它就自动的向后续的使用中传递数据了，这似乎就是 rx 系列代码的思想
;; 为了使流实现能够自动且透明地将流的构建与其使用交错，我们将安排 stream 的 cdr 部分在通过`stream-cdr`程序访问时进行求值，
;; 而不是在通过`cons-stream`构建流时。
;;
;; 这里的 car cdr 指的是 stream 构造出得两个部分，不是那两个操作！
;; stream 其实也就只有 car 和 cdr 两个部分而已
;;
;; 这段话的意思是，在构建一个普通的列表（ordinary list）时，列表中的每一个元素都会被立即求值。
;; 例如，如果你有一个包含表达式的列表，如 `'(1 2 (+ 1 2) (* 2 3))` ，
;; 在这个列表被创建时，所有的表达式（`(+ 1 2)` 和 `(* 2 3)`）都会被立即求值，并且结果（分别是3和6）将被存储在列表中。
;; 然而，对于流（streams），只有当你尝试访问一个元素时，这个元素才会被求值。
;; 也就是说，在流被创建时，并不会立即计算其中的每个元素。
;; 相反，每个元素都会在需要时进行“惰性求值”（lazy evaluation）。例如，
;; 如果你有一个包含同样表达式的流 `(stream 1 2 (+ 1 2) (* 2 3))` ，
;; 那么 `(+ 1 2)` 和 `(* 2 3)` 只会在你尝试访问它们时才会被求值。
;; 也就是只有我们调用了 (stream-cdr stream) 的时候，(cons-stream (1 (+ 2 3))) 中的 (+ 2 3) 才会变成 6 (进行求值)


;; 为了实现这样的 能力, 我们需要引入两个新的概念
;; delay 用于包装某个 exp，不对它直接求值，而是返回一个延时对象
;; force 则是用于强制求得某个延时对象的值
;;
;; 现在的 promise 似乎就和这个 delay 非常类似
(define (cons-stream x y)
  (cons x (delay y)))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

;; 类似于 list-ref 用于求流中的某一项
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (list-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc stream-car s)
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;; 完成原先使用在 seq 上的过程的 stream 版本
;; 可以看到本身的过程组织是没有变化的
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream))))
  )

;; 借助流的能力再来实现原先的求区间内第二个素数方法
;; 在这里，stream-enumerate-interval 返回的东西只有 car 求值了
;; 而 cdr 只是一个 promise。
;; 接下来 filter 里面会看情况去调用 stream-cdr 从而触发后续 stream 数据的求值
;; 也从而会使得内部的 stream-enumerate-interval 再吐出新的值去做判断
;; 直到找到第一个 素数 10007
;; 此时返回 (cons-stream 10007 (delay (stream-filter ..)))
;; 而 (stream-car) 开始调用使得 内部的 delay 开始进一步求值运作
;; 直到某个 (cons-stream 10009 (delay ...)) 再次被计算出来
;; 然后 stream-car 获取到 10009, done!
;; 这样的好处就在于我们的运算和求值是 按需取用 的
;; 减少了无效的中间运算带宽
(define (get-prime-of-interval a b)
  (stream-car (stream-cdr (stream-filter prime? (stream-enumerate-interval a b)))))

(get-prime-of-interval 10000 100000)
