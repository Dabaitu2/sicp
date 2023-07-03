# 3.3  Modeling with Mutable Data

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

####  Sharing and identity



