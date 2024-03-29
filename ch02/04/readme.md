# Multiple Representations for Abstract Data

在这个小节我们主要为了实现针对不同形式的数据实现通用操作 (generic), 引入 类型 以及 面向数据编程 的概念

在最开始的数据抽象章节，我们已经通过抽象屏障控制了复杂度
在通过实现有理数计算的例子中，我们将使用有理数的 API 和有理数的实际表示分离开来
不过，这样的实例的抽象程度还不够强。 这是因为一种数据可能具有多种表达方式。而原来的这种
抽象方式无法通用的应用于同类数据的多种表达。也就是 “多态" 的能力

例如，对于复数的表示就有两种方案：
1. 对于直角坐标系：实部 + 虚部 (real and imaginary parts), 如 2+3i 表示为 (2, 3)
> x 轴为 real, y 轴为 imaginary
2. 对于极坐标系: 模 (magnitude 极径大小) 和幅角 (angle 极角角度)

为了能够使得过程能够足够通用，成为 "generic procedures"
以至于我们可以使之应用于不同的数据表示上。
我们需要引入 "类型标志" (type tags), 这个标志本身就相当于携带了应该如何处理他们的信息
这个类型标志，就和我们常见的 tagged union, 可辨识联合类型差不多了

通过 tagged 实现类型分派, 我们的程序拥有了通用性和模块性。
然而只 通过类型分派去实现通用操作仍然存在问题
1. 一旦出现新的数据表示, 通用操作的维护者必须在代码中显式增加这类表示的处理
2. 其他独立表示的维护者也需要修改和检查代码避免重名。

在数据表示千变万化的实际表示中，这样的操作是繁琐困难的，并且很容易出错。因为我们必须直接去改代码。
更为深入的解释便是，这样的代码设计还不具备 additivity (可加性), 这阻碍了抽象能力, 以及借此进一步构造复杂系统的能力
因此引入了 "数据导向" 编程的概念, 通过实现一系列过程
1. 基于一个二维表格, 将不同的表示 tag(type) 的实际实现(item)注入表格中对应通用操作(op) (put <op> <type> <item>) 
2. 通过 tag, 在调用通用操作时获取到实际表现 (有点虚函数表的意思了?) (get <op> <type>)
(用现代的语言来说，就是运行时函数(动态的,堆上的函数))

不过这样的过程要在第三章才会实现了
