# 3.3  Modeling with Mutable Data  模拟可变数据

第二章处理复合数据(由多个部分组成的数据)的方式来自于使用类似 `cons` 的东西来黏合多个部分。

不管是 list, tree, 还是后面更复杂的 painter, computing systems. 本质上都是利用 `cons` 不停的组合，再通过抽象(利用名字取代具有相同部分的过程/数据, 以便同实际的实现相隔离，从而减少心智负担) 组合的部分来实现复杂数据模拟。

而再落实到方法论上，就是我们对数据定义了 `selector` 和 `constructor` 这两类原子操作来实现数据的构造和数据中某一个子部分的选取。

但到此为止我们所有的代码都可以看成是函数式编程的范畴。这是因为这些所有数据，不管是 painter 还是 complex 都是不可变的。他们所谓的运算或者操作，都是利用 selector 和 constructor 去选择部分，再创建出一个全新的值。这些数据自身也没有状态，是一个固定的静态的值。

而在这一章，我们将针对 “数据变动” 这一种问题进行模拟。也就是说，我们需要增加一种新的原子操作 —— mutator

---

## 3.3.1 可变的 List 结构

针对原先的 list 结构，由于它是由 序对 pair 构成的 ,  我们会引入两个新的操作 `set-car!` 和 `set-cdr!`  来修改 cons 的 car 和 cdr 部分，一个典型的例子如下：

![image-20230628212157965](/Users/tomokokawase/Desktop/Learning/sicp/ch03/02/images/image-20230628212157965.png)

利用这两个原子操作和 `get-new-pair` ，我们也直接实现 cons 方法。

```scheme
(define (cons x y)
  (let ((new (get-new-pair)))
      (set-car! new x)
      (set-cdr! new y)
      new))
```

我们甚至可以自己实现 cons 的 `set-car` 和 `set-cdr`

```scheme
#lang racket

;; 变化 和 赋值 本质上就是一件事，我们在变化一个什么东西的时候就是在赋值
;; 也就是在保证其前后 "相同" 的基础上对这个对象做出了改变, 无论是多么复杂的变化，其根源一定是赋值导致的
;; 比如下面的 set-car! 本质上就是赋值了这个过程中对应的 x 罢了
(define (cons x y)
  (define (set-x! v)
    (set! x v))
  (define (set-y! v)
    (set! y v))
  (define (dispatch m)
    (cond
      [(eq? m 'car) x]
      [(eq? m 'cdr) y]
      [(eq? m 'set-car!) set-x!]
      [(eq? m 'set-cdr!) set-y!]
      [else (error "Undefined operation -- CONS" m)]))
  dispatch)

(define (car z)
  (z 'car))
(define (cdr z)
  (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define hello (cons 2 3))
(car hello)
(cdr hello)

(set-car! hello 'a)
(car hello)
(set-cdr! hello 'b)
(cdr hello)
```

这里要注意的一点是，如果 cons 中存在相同的数据被“共享”， 那么这个时候就要很注意 “同一” 所带来的问题了。

比如我们可能轻易的构造出环形链表，或者在统计链表大小时错误的计算了多次“相同” 的cons。

 ## 3.3.2 Queue

为了实现一个 FIFO 队列，我们通常需要额外维护一个简单 pair 去指向队尾和队首，方便实现入队和出队。这样在面对插入操作时我们可以直接追加到 rear 指向的 pair 之后。而出队时也可以直接修改 front-ptr 的指向即可。

<img src="/Users/tomokokawase/Desktop/Learning/sicp/ch03/03/images/image-20230710212814161.png" alt="image-20230710212814161" style="zoom:67%;" />

## 3.3.3 Tables

<img src="/Users/tomokokawase/Desktop/Learning/sicp/ch03/03/images/image-20230710213108539.png" alt="image-20230710213108539" style="zoom:60%;" />

上面两章都是比较实践性的内容，主要就是在展示利用赋值可以实现一些较为复杂的数据结构

one-dimensional Table looks like above. we accutually still using list(we call that `backbone`) to construct it. the only modification is we store the value by form of a pair rather than single value. That pair is a K-V structure. Meanwhile, to make the subsequent procedure to identify it, the `car` value of the first element of the `backbone` was a dummy value.

> What is `dummy value`?
>
> The term "dummy value" is commonly used in programming and data analysis contexts. It refers to a placeholder or a temporary value that is used to represent missing or irrelevant data, or to serve a specific purpose in a particular situation.

We provide some helper to extract information out of the table.

```scheme
;; one-dimensional table
;; assoc => associate?
;; check if the key was found inside the records
;; recursive
(define ((assoc key records))
  (cond
    [(null? records) false]
    ;; equal can compare multiple type of values: symbol | datum | list
    [(equal? key (caar records)) (car records)]
    [else (assoc key (cdr records))]))

;; find the value from table of specified key
(define (lookup key table)
  (let ([record (assoc key (cdr table))])
    (if record (cdr record) false)))

(define (insert! key value table)
  (let ([record (assoc key (cdr table))])
    (if record
        (set-cdr! record value)
        ;; 新插入的记录放到最前面
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))
```

![image-20230716223809045](/Users/tomokokawase/Desktop/Learning/sicp/ch03/03/images/image-20230716223809045.png)

two-dimensional table is still the abstract of sub one-dimensional tables