#lang sicp

;; a
;; 如下的表达式可以用于计算阶乘
;; 在我们的 eval 实现中也可以做到
((lambda (n)
   ;; 在解析对此 lambda 应用参数(lambda (ft k)) 时
   ;; 1. (lambda ft k) 对应的过程被创建, 环境指向 (lambda (n)) 所创建的环境
   ;; 2. fact 绑定到 1 建立的过程上, (fact fact n) 正常执行
   ((lambda (fact) (fact fact n))
    ;; 3. 2 的过程调用触发对此 lambda 的过程应用参数
    ;; ft 就是自身，因此这里所有参数都能找到环境绑定对应的值
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

;; 同样的，我们稍作替换，也可以实现求 fibonacci 数列
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (fib k)
      (cond
        [(= 0 k) 0]
        [(= 1 k) 1]
        [else
         (+ (fib fib (- k 1))
            (fib fib (- k 2)))]))))
 3)

;; b
;; 避免嵌套递归声明带来的问题的终极方案
;; 就是终极的函数式编程，
;; 1. 不要有任何的状态，只留下 lambda 所有的东西都通过参数传递
;; 2. 除此之外，把一切外部创建的过程都作为参数传递给内部的 lambda，内部的 lambda 永远只使用参数提供的数据, 相当于没有任何副作用
;; 3. 总而言之，就是不依赖词法作用域, 没有 let 去创建过程，而是通过参数传递的方式创建过程
;; 只有永远的 lambda
;; 这个其实也就是传说中的 Y 组合子 hhh, 通过这种方式，lambda 自身也实现了递归调用的能力
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
         true
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
         false
         (ev? ev? od? (- n 1))))))

(f 3)
