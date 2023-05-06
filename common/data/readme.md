# 这个文档里是什么?

SICP 里用到的所有东西都是用最基本的 过程表示的
在 SICP 中，数据也是过程，例如 cons 是一个高阶过程，返回一个取数的过程
而 car, cdr 本质上只是调用这个过程而已 => cons.rkt

利用最最基本的 cons, 我们就可以构造出 list， append 等方法 => sequence.rkt

再基于 list append 我们可以进一步抽象出一些函数式接口 => conventional-interface.rkt

