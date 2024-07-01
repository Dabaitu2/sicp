# Amb 求值器

一般的求值器执行任何一个 scheme 表达式，返回只有三种情况
1. 返回一个值
2. 卡死在那，永远不终止
3. 抛出一个异常

而对于非确定性求值，情况就将变得更复杂



## Execution procedures and continuations

常规的求值器的执行过程中有一个参数：env 即执行环境

然而，对于 amb 求值器，我们需要三个参数: 执行环境，和两个被称为 `continuation procedure` 的过程。

表达式的求值结束后，将会被执行这两个 `continuation procedure` 中的一个：

1. 如果求值出一个结果，则调用 `success continuation` .
2. 如果遇到了死胡同，则调用 `failure continuation`



对于 **success continuation**, 它进行的工作是

1. 接受上面传进来的一个值，并继续计算。
2. 同时，这个 success continuation 会被传入另一个 **failure continuation**,  它会在使用 success continuation 接受值进行计算后产生 dead end 时被调用。

> 这里其实有点像是堆栈的恢复策略。
>
> 我们在执行一个个可能性测试的时候，就像是在向栈中保存当前层级现场。而一旦有一个测试失败，则
>
> 1. 尝试返回上一级，检查还有没有别的可能性，继续试探
> 2. 如果所有的可能性均已经试探结束，则再向上回溯。



对于 failure continuation, 它进行的工作是

试探 Non-deterministic Procedure 的另一种可能分支。

1. 求值器取出一种可能性，将他的值送给 success continuation 过程。
2. 求值器构造出一个新的 failure conitnuation, 也将其送给 succession continuation 过程。

> 这里的第二步就是实现 “回溯” 的关键，我们不是真的倒转历史了。而是将整个历史通过某种联系保留了。
>
> 我们在处理某个层级的求值时
>
> 1. 本质上它就是是上层 success continuation ，上层的 success continuation 本质上被解释器处理为下一步的调用。
> 2. 上层的 continuation 同时还传了一个东西来充当 failure conitnuation，而对于上层来说，本质上这个东西是**（试探下一个值)** 
> 3. 我们在还原副作用时，猜测是保存了原始值，将原始值重新 set 回去



在我们的求值过程中，标识当前的执行遇到 dead end 是通过 amb 表达式来实现的。

同时，如果我们的处理中存在副作用，副作用也应该被撤销。

##### Summary

对于 Failure continuation，它可以由如下情形构造

1. amb 表达式，用户主动触发，标志遇到 deadend
2. driver loop，提供一种机制，如果已经没有选择，则应该报告失败
3. 赋值：拦截失败并且撤销赋值效果

而反过来讲，当一个过程**失败时**，通常是因为：

1. 用户执行了 amb
2. 用户输入 try-again

F ailure continuation 则会在处理失败的过程中被使用。

