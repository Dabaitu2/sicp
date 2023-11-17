# 3.5 Streams

Until Now, we have seen what problems will occur if we introduce Assignment.

1. Complexity with Time concept: the behavior of a program is no longer just determined by input, while what the status itself currently is still matters
2. The difficulty of understanding programs
3. Parallel programming issues: data inconsistent & deadlock
4. Referentially transparent was Broken: 在没有赋值的情况下，一个表达式可以被它所代表的结果替换掉而不改变程序的行为。但是在有赋值操作的情况下，这可能不再成立。

All of those problems are mainly due to we want to simulate this world's change by simulate our computed object with status which changed accompanied with time.

实际上，如果我们不要去让计算机时间和真实的时间形成对应的方式去形成对真实世界的模拟。
我们关心的如果不是 某一时刻某一个值是什么。 而是这整个变化周期内，这个值的变化历史。
那么我们就可以不需要强调这个变化发生在什么时候，只需要关注他们发生的顺序是否正确就好了。

因此，我们可以使用流 和 延时求值 技术来避免 assignment 引入的时间概念所带来的复杂性。
不过 Stream 也是有它自己的问题的。

## 3.5.1 Streams Are Delayed Lists
> 流就是延时的 Lists

在 2.2.3 章节中，我们通过 构造 sequence (序列, 数组).并且为其提供了许多抽象能力 (map, filter, accmulate)
从而实现了对许多不同的操作特征的抽象。

然而原先这样的使用在面对 list 的时候，可能会遇到低效的问题。这也是许多 函数式 代码的问题。
我们的数据从函数中进行传递的时候不可避免地遇到大量复制和构造。这样的操作是**批处理**的。
也就是说，我们在必须完成了第一个函数的所有数据处理，才能传给下一个。

