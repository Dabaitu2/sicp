# 4.4 Logic Programming

## Prelude

> 逻辑编程/程序设计 

计算机科学处理的是 imperative (How to) 的知识, 而数学则处理的是 declarative (What is) 的知识

编程语言要求程序猿通过 step by step 的描述解决特定问题的方法的形式来表述一种知识, 当然在另一方面，很多高级语言也提供了很多方法论知识，去简化实现问题的细节。

大部分程序语言都是围绕对数学函数值的计算组织起来的。面向表达式的语言（如Lisp、Fortran和Algol）利用了“双关语”，即描述函数值的表达式也可以作为计算该值的手段。

>  (也就是说，当我写出一个函数时，本质上我是在描述这个函数想要求一个什么东西，而这个描述本质上就可以用来计算了)



因此, 大部分程序设计语言都强烈的倾向于 单一方向 的运算，也就是计算中有定义清晰的输入和输出。然而，在前面我们也见到了一些在某些方面消解了这种倾向, 例如 3.3.5 中实现的约束系统, 以及 4.3 实现的非确定性计算。在非确定性计算中，由于表达式可能存在多个值，因此我们需要处理的是 relation (关系)
而不是单值函数。也就是说，我们处理的是输入和输出之间的映射的集合。

>  (比如：关系Z => 符合条件 X 的集合 Y)



在本章中，我们将要实现 "逻辑程序设计",  它通过将一种被称为 Unification(统一)的强力符号模式匹配 和 编程的关系视角 相结合。 扩展了非确定性计算的思想。

这种方法在可用时，会变得非常强大。 这种威力部分来自于：

>  一个描述 What is 的知识实际上可以被用来解决多个 How to 组合起来才能解决的问题.



例如，我们在 Lisp 中实现的 append 操作

```scheme
(define (append x y)
    (if (null? x)
        y
        (cons (car x) (append (cdr x) y)))
```

这是一个 How to 的 知识描述

在将其用 What is 的语言翻译后，一种可能的描述是：

1. （if 的 consequence）对于任何一个 list `y`, 对 empty list 和 `y` 作 `append` 形成的就是 `y`
2. （if 的 alternative）对任意的 `u`, `v`, `y`, `z` , 如果 `v` 与`y` 作 append 可以形成 `z` , 则 将 `(cons u v)` 与 `y` 作 `append` 将形成 `(cons u z)`



利用上面的 How to 描述，我们可以解答这样的问题：

> 找出 (a b) 和 (c d) 的 append

可以获得一个必然的解。

然而，这样的 How to 问题却无法解决下面这类问题

1. 找到一个 list y，使它与 (a b) 的 append 可以产出 (a b c d)
2. 找出所有的 x 和 y， 他们的 append 形成 (a b c d)

而我们翻译出来的 what is 可以解决这类问题。

我们要实现的逻辑程序语言，我们将试图让用户在写出一个 append 过程时，让其直接具有 what is 的能力，也就是用户只需要写出 what is，而 How to 由解释器自动地提供，从而使得用户提供的规则可以回答上面提出的所有关于 append 的问题。



## 4.4.1 Deductive Information Retrieval 演绎信息检索

> Deductive: “演绎式”, 在逻辑学和哲学中指从一般到个别的推理过程，即从已知规则推导出特定情况下的结论。在信息检索领域，“演绎式”通常指利用已有知识或规则系统来查询和推理数据。

一个信息检索系统应该包括哪些内容?

1. 已有的知识或规则, 类比于数据库中存储的数据, 例如下文中存储的一些 lisp 格式数据

   ```scheme
   (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
   (job (Bitdiddle Ben) (computer wizard))
   (salary (Bitdiddle Ben) 60000)
   ```

2. 简单查询的能力: 它利用模式匹配去数据库中寻找符合查询模式的数据

   ```scheme
   (job ?x (computer . ?type))
   ```

3. 复合查询的能力: 它可以对简单查询按照逻辑方式进行组合, 例如 and, or, not 等

   ```scheme
   (and (job ?person (computer programmer)) 
        (address ?person ?where))
   ```

4. 将逻辑看作程序的能力:  利用 **Rule 规则**, 我们可以将查询语言按照规则定义的查询条件进行匹配, 并推导出是否可以满足规则的结论的能力.

   ```scheme
   (rule (lives-near ?person-1 ?person-2)
   			(and (address ?person-1 (?town . ?rest-1)) 
              (address ?person-2 (?town . ?rest-2))
              (not (same ?person-1 ?person-2))))
   ```



## 4.4.2 How the Query System Works? 查询系统如何工作?

参考: [4.4.2](./02-how-the-query-system-works/readme.md)

## 4.4.3 Is logic-programming mathematical logic? 逻辑程序语言和数理逻辑等价吗 ?

参考: [4.4.3](./03-is-logic-programming-mathematical-logic/readme.md)

## 4.4.4 Implementing the Query System 查询系统的实现

参考 [4.4.4](./04-implementing-the-query-system/readme.md)

