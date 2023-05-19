## 符号代数

在本章我们要实现多项式的算术

### 什么是多项式

它表现为 $5x^2 + 3x + 7$, 即未定元 (indeterminate）是 x 的多项式
分析其定义：

> We will define a poly- nomial to be a sum of terms, each of which is either a coefficient,
> a power of the indeterminate, or a product of a coefficient and a power of the indeterminate.
> A coefficient is defined as an algebraic expression that is not dependent upon the indeterminate
> of the polynomial.

多项式是 **项** 的和式, 每一项都是 一个系数 / 未定元 (在例子里就是 x) 的乘方 / 系数和未定元乘方的乘积
而系数定义为 一个代数表达式，但它不依赖于这个多项式的未定元，也就是比如我们定义了多项式是基于 x 的多项式，
系数中就不可以再出现 x

在这样的定义下：
$5x^2 + 3x + 7$ 和 $(y^2 + 1)x^3 + (2y)x + 1$
都是合法的多项式

而多项式算术正是这些多项式的计算

为了教学和实现简单，多项式均需要具有相同的未定元，并且
只考虑多项式的加法和乘法, 同时，我们强制定义在我们的演算系统中，
多项式的表示是一种语法形式而非数学意义
也就是说, $5x^2 + 3x + 7$ 和 $5y^2 + 3y + 7$ 不代表相同的东西
即使在数学意义上他们是相同的

结合习题我们可以实现针对单变元多项式的加减乘除
