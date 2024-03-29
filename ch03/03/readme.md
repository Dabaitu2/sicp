# 3.3  Modeling with Mutable Data  模拟可变数据

第二章处理复合数据(由多个部分组成的数据)的方式来自于使用类似 `cons` 的东西来黏合多个部分。

不管是 list, tree, 还是后面更复杂的 painter, computing systems. 本质上都是利用 `cons` 不停的组合，再通过抽象(利用名字取代具有相同部分的过程/数据, 以便同实际的实现相隔离，从而减少心智负担) 组合的部分来实现复杂数据模拟。

而再落实到方法论上，就是我们对数据定义了 `selector` 和 `constructor` 这两类原子操作来实现数据的构造和数据中某一个子部分的选取。

但到此为止我们所有的代码都可以看成是函数式编程的范畴。这是因为这些所有数据，不管是 painter 还是 complex 都是不可变的。他们所谓的运算或者操作，都是利用 selector 和 constructor 去选择部分，再创建出一个全新的值。这些数据自身也没有状态，是一个固定的静态的值。

而在这一章，我们将针对 “数据变动” 这一种问题进行模拟。也就是说，我们需要增加一种新的原子操作 —— mutator

---

## 3.3.1 可变的 List 结构

针对原先的 list 结构，由于它是由 序对 pair 构成的 ,  我们会引入两个新的操作 `set-car!` 和 `set-cdr!`  来修改 cons 的 car 和 cdr 部分，一个典型的例子如下：

![image-20230628212157965](../02/images/image-20230628212157965.png)

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

<img src="./images/image-20230710212814161.png" alt="image-20230710212814161" style="zoom:67%;" />

## 3.3.3 Tables

<img src="./images/image-20230710213108539.png" alt="image-20230710213108539" style="zoom:60%;" />

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

![image-20230716223809045](./images/image-20230716223809045.png)

two-dimensional table is still the abstract of sub one-dimensional tables

## 3.3.4 Circuit Simulator

本章依然是一个实践性内容，通过 mutator 和赋值来实现电路模拟。

我们结合之前所提到的诸多最佳实践来进行我们的模拟过程

### Wishful Thinking + 组合过程

首先是 wishful thinking： 我们认为我们需要什么基本构件来实现我们的电路系统，并且假设它们已经实现了。

#### 基本构件和过程

在这里，我们认为我们需要基本构件 `wire` ,

基本过程 `（make-wire) ` `(get-signal wire)` , `（set-signal! wire signal)` 和 `(add-action wire action)`

Wire 是一个持有 signal 的对象, 这里看起来有点反常识，我们总认为 wire 只是流过 signal，但是在模拟的过程中，它持有 signal 是最方便的。

**get-signal**： 从 wire 中获取当前信号

**set-signal**：改变 wire 中的信号

**add-action**： 当 wire 的信号改变时触发 action

#### 构造基本电路单元和复合电路单元

通过这些基本构件，我们可以构造出电路的基本元件，如 `inverter `  (反门)， `and-gate` (与门)， `or-gate` (或门)。

同时，再基于这些基本单元，我们可以实现 half-adder 和 fall-adder. 以及习题中提到的 ripple-carry-adder 等。

### 利用赋值实现基本构件

现在，让我们回到 wire 的构造，我们通过赋值和消息传递来实现 “持有一个值” 的 computational object 即 wire。

以及其相关的基本过程。



## 利用 Agenda 模拟时间流动

我们手动操控信号的过程，需要依赖时间流动才可以传导到其他的 wire，因此我们需要有一个 "时间模拟器"
在这里我们可以引入 Agenda 这样一个概念来模拟时间流动。agenda 并不是真的模拟了时间一秒一秒的流动。
而是一个 cons, 如下所示

```scheme
(define agenda (cons current-time time-segements))
(define time-segement (cons time queue))
```

第一项当前的时间，agenda 模拟时间流动的方式并不是一秒一秒的计数，而是一次运行一个 time-segement,
time-segment 会告诉 agenda 此时应该是什么时间, 并且会执行 segement 所包含的所有任务。


## 3.3.5 Propagation of Constraints

the general procedure we used is just a one-directional computation
like (define c (+ a b)) => c = a + b,
we couldn't get value of `a` if we know `b` & `c`, even through we know there do have such relation

by putting forward the concept of `constraint`
we want to build a system which can describe such relation
and can get any part of the relation if other necessary parts are ready
so if we know c and a, we can get b immediately

constraint system is very similar as our circuit simulator
but it's acutally more simple cause we don't need to care about latency and agenda

all changes will be put into effect immediately (it's a hagiographic system)
an example looks like belowing chart, every large rect is a `constraint` (the primitive unit of our system)
and they can be connected by conncetor which looks like the circuit simulator's *wire*



                      relationship between Fahrenheit and Celsius temperatures
    
                                       9C = 5(F - 32)


             ┌──────────────┐           ┌──────────────┐            ┌──────────────┐
             │              │           │              │     v      │              │
       C ────┤ m1           │           │           m1 ├────────────┤ a1           │
             │              │    u      │              │            │              │
             │       *    P ├───────────┤ P     *      │            │       +    s ├──── F
             │              │           │              │            │              │
         ┌───┤ m2           │           │           m2 ├──┐      ┌──┤ a2           │
         │   │              │           │              │  │      │  │              │
         │   └──────────────┘           └──────────────┘  │      │  └──────────────┘
        w│                                              x │      │ y
         │     ┌──────┐                           ┌────┐  │      │  ┌─────┐
         └─────┤ 9    │                           │ 5  ├──┘      └──┤ 32  │
               └──────┘                           └────┘            └─────┘

#### How does it work?

```scheme
(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(celsius-fahrenheit-converter C F)
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

;; tells C that this directive comes from the user.
(set-value! C 25 'user)
;; Probe: Celsius temp = 25
;; Probe: Fahrenheit temp = 77
;; done

;; (set-value! F 212 'user)
;; Error! Contradiction (77 212)
;; cause the constraint system has hold the value for connector C and F

(forget-value! C 'user)
;; we can let the conncetor C forget it's value of 'user, and the related F will be forgot too
;; Probe: Celsius temp = ?
;; Probe: Fahrenheit temp = ?
;; Probe

(set-value! F 212 'user)
;; Probe: Fahrenheit temp = 212
;; Probe: Celsius temp = 100
;; done

;; this system works for both C -> F and F -> C
;; nondirectionality is the distinguishing feature of constraint-based systems
```

