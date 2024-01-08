#lang sicp

(#%require "../../../common/data/stream.rkt")

;; 下面通过求阶常微分方程的数值解为例子来说明有的例子我们可能
;; 需要显式的调用 delay

;; 求一阶常微分方程本质上就是求 dy/dt = f(y)
;; 这里之所以是 t 不是 x，是因为我们在计算机中模拟这个过程，而这个过程
;; 往往是用电路上的时间走向来模拟的
;; 在已知导函数 y`(x) = f(x) 的情况下
;; 找出原函数 y(x)
;; 常用的方法便是对两边做积分
;; 得到 y = ∫f(x)dx + C
;; 也就是说，y 的值，就是对 f(x) 求积分的值罢了

;; 下面是对求一阶常微分方程的一种尝试
;; f 是 f(y), 也就是 y 的某个函数 dt 是时间的极小增量
;; y0 是初始值, 也就是不定积分的常数项
;; y 是输出，代表这个原函数对应的输出流,

;; 通过上面的定义解释，本质上我们就是要求出导函数的积分结果流
;; 而这个积分结果流依赖于导函数的每个 dt 下的产生的 dy 形成的流，也就是 f 的输出流
;; f 的输出流本质上又是对于 y 的输出进行运算得到的一个流 (y 的输出的差值流)

;; (define (failed-solve f y0 dt)
;; 通过差值流形成的导函数去求原始函数
;; (define y (integral-stream dy y0 dt))
;; ;; 想要获得 y 的输出流的差值, 也就是导函数
;; (define dy (stream-map f y))
;; y)

;; 上面的这种 solve 方法目前是不可能完成的。这是因为想要求出 y， 就必须先有 dy
;; 而想要有 dy 又依赖 y，就像是一种死锁
;; 在即时求值的情况下。是不可能的

;; 不过，这个思路在理论上是行得通的，因为我们的 integral-stream 内部使用的 cons-stream
;; 的底层通过 delay 确实提供了 延时操作，我们确实可以在不知道 dy 的情况下先获得 y 的第一个值
;; 然后再通过 stream-map 获取 y 的第一个值去产生 dy 的第一个值，从而使得这个循环驱动起来
;; 从而打破死锁

;; 因此我们必须要显式的引入 delay 去对其中某一个部分先进行延时计算
;; 为此我们可以修改 integral-stream, 使其接受一个 delay 的流，因为我们第一步其实不需要这个流的数据的
;; 这样，我们可以将参数也进行延时包裹，避免由于第一步参数必须求值导致无法推进这类逻辑的问题
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ([integrand (force delayed-integrand)])
       (add-stream (scale-stream integrand dt) int))))
  int)

(define (solve f y0 dt)
  ;; 不这样做会报错，可能跟 racket 实现有关系 
  (display "")
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define x (solve (lambda (y) y) 1 0.001))

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
