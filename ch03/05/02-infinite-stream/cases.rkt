#lang racket

(#%require "../../../common/data/stream.rkt")
;; 流延时求值的特性结合递归调用，可以构造出无穷流，从而等价于一个无穷长的 sequence
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

;; 这样是很优雅的概念，就像我们用数学符号表示出一个无穷的数列一样
;; 通过计算机语言我们同样可以构造出这样的无穷概念，而且还是可以交互的
(define integers (integers-starting-from 1))

;; 辅助函数
(define (divisible? x y) (= (remainder x y) 0))

;; 我们还可以利用流的界面操作产出另一些无穷流
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))


;; 流的精妙之处就在于，它是延时求值，按需求值
;; 只要 car 时才会求值，要多少用多少。
;; 可以任意组装和产出新的流
;; 而它本质上只是用 lambda 做了一层隔离而已，实现的非常优雅
;; ps: lambda 就是一种特定规则的表达式，理论上使用 lambda，符号规则
;; 以及抽象和组合，函数为一等公民这些特性组合。可以将任何计算机程序表述出来。
;; lambda 基本上就满足这样的表现: lambda <自变量> <表达式>
;; 自变量和表达式是一些基本符号规则, 或者 lambda 表达式的组合

(stream-ref no-sevens 100)

;; 利用无穷流还可以实现无限生成 fibonacci
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))
(stream-ref fibs 1)
(stream-ref fibs 2)
(stream-ref fibs 3)
(stream-ref fibs 4)
(stream-ref fibs 5)


;; sieve of Eratosthenes. 厄拉多塞筛法，用于确定某个大数是否是素数
;; 这个算法公元前3世纪就已经提出，但直到20世纪70年代其效率才被概率算法(基于米勒拉宾检查)超过
;; 其基本思想就是，我们从第一个质数 2 开始构造一条无穷整数流
;; 然后从其中筛掉 2 的所有倍数，因为它们肯定不是质数
;; 这时会形成一个从 3 开始的流，我们重复同样的操作，筛掉 3 的所有倍数
;; 形成一个从 5 开始的流，以此类推..
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50) ;; 233

(display "---------------cases ends--------------")
(newline)
(#%provide integers-starting-from divisible?)
