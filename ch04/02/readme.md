# 4.2 Variations on a Scheme — Lazy Evaluation

> Scheme 的变形 - 惰性求值

当我们要想发明一种新的语言时，一种快速制造原型的方法就是利用已有的语言实现这个语言的一个求值器。这样我们可以快速的去试验这个语言的一些新特性。而规避提早面对在低级语言或者硬件语言中去做出完整实现的麻烦事情。

本章的主要目的就是提供了 Scheme 的几种变形，从而提供了额外的描述能力。



## 4.2.1 Normal Order and Applicative Order

> 正则序与应用序

一般的 Scheme 采用应用序，也就是 application 应用参数的时候需要先对参数求值。这也就是说下面的代码无法正常的被解析。

```scheme
(define (try a b)
  (if (= a 0)
      1
      b))

（try 0 (/ 1 0))
```

因为 `(/ 1 0)` 在 if 中即使不会被使用到，由于参数被迫求值，这里也会提前抛出异常。

如果能够让我们的语言实现规范序，那么下面的代码就会很有价值,  因为即使我们提供的某些参数本身可能会产出错误，但由于它们不被预先求值. 也可以保证逻辑的正常运行，就像是现代语言中常见的 try-catch 语句一样

```scheme
(define (unless condition
          usual-value
          exceptional-value)
  (if condition exceptional-value usual-value))

(unless (= b 0)
  (/ a b)
  (begin
    (display "exceptional: returning 0")
    0))
```



如果在参数进入 body 前需要被求值，那么我们认为这个过程对于参数是 “严格(strict)” 的,  反之则是 non-strict 的。

在纯的应用序语言中，所有的参数都是 strict 的

在纯的规范序语言中，

1. 所有的复合过程参数都是 non-strict 的
2.  对于基本 primitive 过程，其参数可能是 non-strict 也可能是 strict 的 

还有语言甚至可以支持在参数中增加一些控制能力, 从而使得参数可以自行决定是不是 strict 的

如果我们可以在构造数据结构的时候将过程作为 non-strict 的，比如对于 cons 我们这么做,  那么 我们就可以在还不知道元素的具体值的情况下 去完成一些有用的计算 例如获取 list 的长度. 



## 4.2.2 An Interpreter with Lazy Evaluation

本章节将复合过程改编为 Normal Order 也就是非严格的,  而对于基本过程则保持 Application Order 也就是说严格的

基本策略为: 在 Apply 一个 Procedure 时，解释器会判断出哪些参数需要 Lazy-Evaluation, 对于需要 Lazy 的参数，均不求值，而是将其包装为一个 **Thunk**, **Thunk** 包含这个参数对应的表达式，以及求值这个 Procedure Application 时所处的 Env .

同样，我们需要实现一个类似于第三章提到的 `force` 操作来实现求值一个 Thunk

调用 `Force` 的时机通常是我们需要获得这个值时才进行 (换言之, 这是惰性求值的关键, 也就是我们要明白这些值什么时候才会用到)，这包括：

1. 应用基本过程需要这个值
2. 它将作为条件表达式的 predicate 谓词时
3. 将它作为一个过程 procedure 去 Apply 时 也就是 (add 1 2) 里的 add 必须要求得实际对应的 body



然后，我们选择将这个 thunk 实现为 memorized thunk, 也就是带缓存的，这的选择也和前面第三章所讲的一致,  可以减少很多无用的开销，不过，这样也带来了其他的问题



```diff
(define (eval exp env)
  (cond
    [(self-evaluating? exp) exp]
    [(variable? exp) (lookup-variable-value exp env)]
    [(get 'exp (car exp)) ((get 'exp (car exp)) exp env)]
    [(application? exp)
-    (apply (eval (operator exp) env)
-           (list-of-values (operands exp) env))]
+    ;; 当 exp 需要作为 apply 的 procedure 的 body 被调用时, 我们必须求出其实际值
+    (apply (actual-value (operator exp) env)
+           (operands exp)
+           env)]
    [else (error "Unknown expression type: EVAL" exp)]))

;; 在 apply 中，修改对应的部分
(define (apply procedure arguments env)
  (cond
    [(primitive-procedure? procedure)
     (apply-primitive-procedure
      procedure
+     ;; 基本过程获取实际值, 因为我们希望他是 application order
+     (list-of-arg-values arguments env))] ;; changed
    [(compound-procedure? procedure)
     (eval-sequence (procedure-body procedure)
                    (extend-environment
                     (procedure-parameters procedure)
-                    arguments
+                    ;; 复合过程的参数获取包装后的 thunk, 这部分就是 normal order 了
+                    (list-of-delayed-args arguments env) ;; changed
                     (procedure-environment procedure)))]
    [else
     error
     "Unknown procedure type -- APPLY"
     procedure]))

```



Thunk 的基本实现如下:

```scheme
;; 为了实现 force-it 对应的功能，我们需要实现一个数据结构：thunk
;; ============ thunk ============
;; thunk 是用于实现惰性求值的数据结构
;; 它包括一个表达式和这个表达式对应的环境
;; delay-it 其实就是 make-thunk
(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk)
  (cadr thunk))

(define (thunk-env thunk)
  (caddr thunk))

;; 被记忆过的值，存储的被求出的值而非原始表达式
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr (evaluated-thunk)))

;; 获得实际的值, 在 normal order 中只有少许情况会使用它
(define (actual-value exp env)
  ;; 将 eval 出来的结果强制求值
  (force-it (eval exp env)))

;; 求值 chunk 中的实际东西
(define (force-it obj)
  (cond
    [(thunk? obj)
     (let ([result (actual-value (thunk-exp obj)
                                 (thunk-env obj))])
       (set-car! obj 'evaluated-thunk)
       (set-car! (cdr obj) result)
       (set-cdr! (cdr obj) '())
       result)]
    [(evaluated-thunk? obj) (thunk-value obj)]
    [else obj]))
```



## 4.2.3 Streams as Lazy Lists

基于 1. 2 小节所给出的 惰性求值 的 evaluator 实现, 我们甚至不再需要显式的使用特殊形式 `cons-stream` 或 `delay` 去构造无穷流,  因为其实这两者作为特殊形式本身就有一些缺陷：

1. 无法和高阶过程一起使用

   > Quiz 4.26 unless 实现为特殊形式带来的问题
   >
   > 将 unless 实现为一个语法结构，会导致 unless 无法被作为一等公民传入, 正如我们不能把 if 当成参数传递给其他高阶过程一样
   > 因为 unless 必须和它对应的语法结构一起出现 (在底层表现为一个 `(cons 'unless ...)` ), 而不可以仅仅传入这个标识 (一个 symbol)
   > 否则，它会被命中为 variable 变量，而 env 中是没有这个所谓的 unless 变量的.
   >
   > 这个问题对所有的特殊形式都一样.

2. 流必须创建为一类特殊的对象，并且重新实现 序列相关的诸多接口. 

   > 这个很好理解, stream 本身所需要的接口不能简单地使用 append, map 等序列接口, 而是要使用专用的 stream-append, stream-map 等

那么我们可以用什么策略来替换呢

1. 我们可以改变 `cons` 的定义，扩充惰性求值器，将 `cons` 变为一种 non-strict 的操作
2. 我们可以不把 `cons` 实现为基本过程(primitive), 而是通过**一般过程**去实现

这样, 理论上我们就可以通过扩充 list 的能力, 使之天然的兼容 stream 类的操作, 而可以基于同样的接口实现所谓的 “多态” 了.

下面就是基于上述 1+2 策略所实现的 cons, car 和 cdr.

```scheme
(define (cons x y)
  (lambda (m) (m x y)))

;; z 这个 cons 实际上是个函数， 而 (lambda (p q) p) 作为 实际的 m 被提供给 cons 就可以获得闭包缓存的值
(define (car z)
  (z (lambda (p q) p)))

;; 同理，不多赘述
(define (cdr z)
  (z (lambda (p q) q)))
```

而如果将其嵌入我们的 evaluator 中也很简单

```scheme
(define (setup-environment)
  (install-special-form-package)
  (install-derived-form-package)
  (let ([initial-env (extend-environment
                      (primitive-procedure-names)
                      (primitive-procedure-objects)
                      the-empty-enviroment)])
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'nil '() initial-env)
    ;; 还有一种更简单的写法，来自 https://www.inchmeal.io/sicp/ch-4/ex-4.33.html
    ;; 直接使用 actual-value 去预先求值我们支持的表达式
    ;; 从而轻松地注入全局函数
    (actual-value
     '(begin
        (define (cons x y)
          ;; 这就是类似于 JS 中的 [native code]
          ;; 注意，这里产生的就是解释器内部所使用的 cons
          ;; 但是我们要通过我们注入的 underlying-car / cdr 才能访问
          (underlying-cons 'lazy-cons (lambda (m) (m x y))))
        ;; 这里接收到的 z 就是一个 underlying-cons
        ;; 用户一般的 car / cons 其实是要加一层胶水才能访问的
        (define (car z)
          ((underlying-cdr z) (lambda (p q) p)))
        (define (cdr z)
          ((underlying-cdr z) (lambda (p q) q))))
     initial-env)
    initial-env))
```



同样借助这样的过程, 我们原先实现的 list, 就兼具了有穷数据和无穷流的两种能力

```scheme
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

;; 这里的流不再需要用 cons-stream 了, 因为 compound procedure 的参数本身就是 lazy 的, 自然的使得当前基于惰性流形成了无穷流.
(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))
```



基于完全延时的 cons 过程，我们求 3.5.4 节的积分时也不再需要手动写一个 delay 特殊形式了
同时，我们也不需要单独的去使用 stream 相关的惰性求值 api 去将它和 list 分开了

```scheme
(define (integral integrand initial-value dt)
  (define int
    (cons initial-value
          (add-lists (scale-list integrand dt) int)))
  int)

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y)
```
