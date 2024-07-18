# 利用 Non-deterministic Computing Evaluator 实现 Query Language

1. 由于 amb 可以自动遍历所有可能的值， 所以处理 frame 相关的方法都只用关心单个 frame 而不是 frame 的流
2. 不过插入的信息还是可以存放在 stream 里面 
3. 当标志匹配失败的时候，我们用 `(amb)` 替代返回 `fail` 以驱动 evaluator 自动的完成下一个尝试


