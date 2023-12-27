#lang sicp

(#%require "../../../common/data/stream.rkt")
;; the question is under a huge context
;; which related to 2.5.3 use list of coefficient to represent polynomial
;; but here we don't care the polynomial itself, we just care about
;; coeff's stream
;; a)
(define (integrate-series stream)
  (stream-map / stream integers))

;; b)
;; a^x 的导数是 a^x * ln(a)
;; 因此 e^x 的导数 是 e^x 本身
;; 对于对于一个在实数或复数 a 邻域上，以实数作为变量或以复数作为变量的函数，
;; 并且是无穷可微的函数 f(x)，它的泰勒级数是以下这种形式的幂级数：
;; 无穷可微就是函数光滑，在任意原点上光滑连续，没有间断点，没有阶跃性的点
;;  ∑_{n=0} f^(n)(a) * (x-a)^n / n!
;; 这里 n! 为阶乘, f^(n)(a) 为 n 阶导数在 a 点的值 (理论上 n 阶导数需要递归求解，不过我们有常用的高阶导数公式)
;; 这里不多讲
;; 如果 a = 0, 这个级数可以被称为麦克劳林级数
;; 由于 e^x 的 1 阶导数是它本身，因此它的 n 阶导数都是它本身
;; 所以其麦克劳林级数为  ∑_{n=0} f^n(0) * x^n / n!
;; 展开为：1 + x + x^2/2! + x^3/3! +...
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1
               (stream-map -
                           (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
