# Modularity, Objects, and State

## prelude

In previouse sections we understand how **abstract** powerfully combine primitive _procedure_ and _data_ to compound entities.

Abstract can help us to overcome the some of the complexity problems on builing a large systems. But it's not fair sufficient because by abstract we can only combine logic inside single place (file), we need a hyper view of constructing whole program.

So we need _modular_ system to help us develop program's different parts separately.

### A case: Object Oriented Modeling Physical Systems

A powerful strategy for building system to model physical systems is to build a corresponding _Computational Object_ for each original objectoriginal object. and every actions of the original object will get a `symbolic operation` (like `(put 'add add)`).

The ideal assumption of this model is:

1. if we have to add a new computational object, we only need to add this computational object on code rather than change the whole system strategically.
2. If we need to extend a new _action_, we just need to add the related symbolic actions of that.

If those can be done, we can confine our testing and checking just on our
modificated parts locally.

### Object Oriented & Streams Oriented ways

Object Oriented Model will organize whole system as the collection of distinct objects whose behavior may change over time.

Streams-based organizational strategy concenrates on the streams of information that flow in the systems, much as an electrical engineer views a signal-proceessing system.

Both ways will raise significant linguistic issues in programming.

1. For Object, cause the Object's status can be changed, we need to keep track of this object thus the original _substitution model_ of computation is no longer fitful, we need to use _environment model_ of computaion. Meanwhile, we need to take account of the influence of _time_.

2. to tackle with those issue with time, we can try using _streams_ model which can decouple _simulated time in our model_ from _the order of the events that take place in the computer during evaluation_. We will accomplish this using a technique known as delayed evaluation.





## 模块化，对象和状态

第一二章中介绍了

1. 组成程序的基本元素
2. 通过组合基本过程和基本数据的方式构造复合实体 (数据或过程) 
3. 通过抽象来克服构造大型系统所遇到的复杂性问题

不过，这些对于程序设计而言还不够用。我们还需要通过一些组织原则来指导我们系统性地完成系统的整体设计。

特别是需要一些帮我们构造起**模块化**的大型系统的策略，也就是是说使得这些系统能够自然的划分成可以分别进行开发和维护的部分。



一种有力的设计策略很适用于构造模拟真实物理系统的程序，也就是基于**被模拟系统的结构去设计程序的结构**。对于有关的物理系统里的每个对象 object，我们构造一个与之对应的计算对象，对该系统里的每种动作 action，我们在自己的计算系统里定义一种操作。

这样的目标是，在面对我们所模拟的系统中引入的新对象或新操作时，我们只需要扩充对应的对象或动作即可。从而保证程序修改的精确性。不需要修改整个程序。(高内聚，低耦合)。



而为了实现这样的目标，本章提出了两种特点鲜明的组织策略：

1. 基于对象模拟的组织策略。将一个大型系统看作一大批对象，他们的行为可能随着时间而不断变化。
2. 基于流的组织策略。将注意力集中在流过系统的信息流上。



这两种组织策略的引入，都对程序设计提出了重要的语言层面的支持要求。（也就是说原来我们所提到的程序的基本组成，组合和抽象，代换模型已经不够用了）

1. 对于对象组织策略，我们需要关注
   1. 对象 “如何变化”。 同时 变化后的对象依然 保持其标识。**也就是说，在我们的认识中，这个变化了的对象还是那个对象，而不是说变成了另一个对象， **更抽象的说，就是当我们两次观测某个对象时，我们发现它的属性，状态已经不一致了，我们如何确定这其实是**同一个对象通过”变化“形成的**， 而不是**我们观察的其实是两个不同的对象。** 我们将要引入局部状态变量和“赋值” 这两种能力来实现这种能力 (3.1 节)。然而，在带来模块化增益的同时。这使得优美的代换模型将不再使用，取而代之的是引入一个较为复杂的 "环境模型"。(3.2 节). 本章通过对象模拟，引入状态和赋值，实现了一些可变的数据结构 (3.3 节)
   2. 如何处理程序执行的并发问题。当多个对象同时相互交互，对状态做修改，读取时，如何保证结果的正确。我们需要引入 序列化 操作 (serializer) 和 互斥量 (mutex) 来解决这类问题。
2. 对于流的组织策略。我们需要消解 “时间” 和 “顺序” 之间的关联关系。我们只在乎顺序而非“时间”。因此我们将通过**延时求值**的策略来实现。
