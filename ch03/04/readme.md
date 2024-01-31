# 3.4  Concurrency: Time Is of the Essence 并发：时间是本质问题

## 3.4.1 The Nature of Time in Concurrent Systems

在引入赋值操作之后，我们的对象模型将被迫和时间开始产生关联。

这在我们试图在程序中实现并发操作时会带来一些麻烦。

由于在现实事件中，我们定义的针对状态进行修改的操作在出现并发时其发生顺序是不固定的，这意味状态在操作发生时（写入/读取）需要严格的遵照其实际发生的顺序来变化。同时操作也需要严格的按照其实际发生的顺序来变化。

在引入赋值后，赋值操作对于状态的变化和时间的关联。导致我们需要在处理赋值操作时非常小心，严格按照实际发生的顺序来操作。特别是在我们关心中间状态而非最终状态时，这样的要求就更加严格。



## 3.4.2 Mechanisms for Controlling Concurrency

### 使用串行化来控制并发

为了保证不同的进程中交错执行的各种事件。基于对象模拟和赋值的组织策略下，我们有一些解决方案试图来处理这些问题。例如 “Serializer” (串行处理器)。

串行处理器的本质就是，当我们的事件中的操作涉及到针对**获取某个已有的值并且对其赋值**时。我们需要保证这两个操作应该在同一个过程中执行。同时，其他任何对变量赋值的操作都无法在这个过程中并发执行。这实际上就是现代编程中常用的加锁的概念。在我们的视线中，我们会利用一个 serializer 来包裹我们需要加锁的行为，使其返回的过程被锁保护。（serilizer 实际上就是一个高阶函数).



### 并发访问多个资源

串行处理器在对于单个资源的并发访问上表现很良好。然而如果我们的问题扩展到对多个关联共享的资源进行并发访问时，依然存在着程序设计上的困难。

例如，想象一个取款操作，假设我们有三个账户 a1, a2, a3, 用户1想要交换 a1, a2 的余额， 此时用户2想要交换 a2, a3 的余额。假设 a1-a2 的交换发生在 a2-a3 的读写之间。此时 a2, a3 还没有加锁。而 a1-a2 已经把数据给改掉了。这次 a2-a3 的交换结果就不再正确。

为了解决这样的困难，我们可能不得不在针对每一个资源进行串行访问的过程前暴露当前 serializer 将锁暴露出阿里, 而不是让各个模块自己承担对访问进行加锁包装的操作。（这其实破坏了模块化)

然后，我们需要多个 serializer 在更高一层进行组合去保证多个资源访问的先后次序。在上面的例子中，我们就可以通过对进行 exchange 的两个资源全部加锁。来实现对两者共同的保护。这是在单个账户内部无法实现的。



### 死锁问题

通过暴露串行化处理来并发访问多个资源实际上仍然存在一定问题。依然是同样的转账过程，假设我们的两个用户并发的尝试交换账户 a1, a2 两者分别获得了 a2, a1 的锁，然而彼此都需要另一个人释放锁才可以推进程序。这样的相持就是死锁。

一种可能的解决方案是针对每个资源进行编号，使得进程优先进入具有较低编号的资源。也就是 a1, a2 交换时，不管是 a1 发起 a2 交换还是 a2 发起 a1 交换，都是先从 a1 进行加锁。

这种策略对于解决交换账户的问题可行，然而对于某些场景，死锁需要更复杂的技术去解决，甚至是无法避免的。

假设系统中有两种类型的资源，并且每种资源都有多个实例。例如，系统中有打印机和扫描仪两种资源，每种都有若干个可供使用。进程A需要首先获得打印机然后获得扫描仪以完成其工作；进程B则相反，它需要首先获得扫描仪然后获得打印机。

如果我们通过给这些资源指定唯一标识编号并强制进程遵循这一编号顺序来获取资源，则可能会出现以下情况：

1. 进程A获得了一个打印机。
2. 进程B获得了一个扫描仪。
3. 进程A尝试获取扫描仪但必须等待因为B已经持有一个。
4. 进程B尝试获取打印机但必须等待因为A已经持有一个。

即使我们按照标识编号排序来请求资源，但由于每类资源都可能包含多个实例且它们之间没有特定的排列顺序（例如所有打印机都被视为等价），就无法保证避免死锁。

在更复杂的系统中，可能存在更多类型和实例的资源以及更复杂的依赖关系。如果不能将所有资源归入单一线性顺序或者对于某些操作无法事先知道所有需要使用到的资源，则上述方法可能就不足以解决死锁问题。

再比如，在分布式系统中，由于网络延迟或者部分信息不可用等原因，可能难以确保全局一致性来对所有资源进行排序。此外，在动态变化的环境下（例如新设备和服务可以随时被添加到系统中），为新加入系统的元素分配唯一标识并让所有参与方知晓也是一个挑战。