# Symbolic Data

All the compound data objects we have used so far were constructed ultimately from numbers.
In this section we extend the representational capability of our language
by introducing the ability to work with arbitrary symbols as data.

### 01 Quotations 引号

将数据从 数值 扩充到 "符号" 需要我们支持 为 数据对象(data object) 加引号(quotations)
因为只有这样，我们才能知道这个对应的是字面量本身，而不是这个符号关联的数值
比如 ((Norah 12) (Molly 9) (Anna 7)) 中的 Norah / Molly / Anna 都是字面量，而不是一个数值的标识符
在 scheme 中，可以利用 'a 的形式来做

允许在一个语言中使用引号号，将会极大地损害根据简单词语在语言中做推理的能力，
因为它破坏了对等的东西可以相互替换的观念 。
举个例子, 三等于二加一
但是“三”这个宇却不等于“二加一”这个短语。

引号是很有威力的东西，因为它使得我们可以构造起一种能操作其他表达式的表达式 (正如我们将在第 4 章里看到的那样)。
但是，在一种语言里允许用语句去讨论这一语言里的其他语句，
那么我们就很难维护“对等的东西可以相互代换”的一致性了。

举例说，如果我们知道 长庚星就是启明星， 那么我们就可以从句子“长庚星就是金星” 推导出“启明星就是金星”。
然而，即使有 “张三知道【长庚星就是金星】” ，我们也无法推论说“张三知道【启明星就是金星】” 。
也就是在有未知上下文的情况下，我们无法轻易对 equals 做出代换操作了

#### 02 实例: 符号求导

#### 03 实例: 表示集合
在前面的实例中，我们已经尝试了 有理数 和 代数表达式 这两种复合数据对象的表示.
对应这两个实例，我们都采用了某一种选择，在构造时或者选择成员时去简化 (约简)有关的表示。除此之外，选择用 list 的形式来表示这些结构都是直截了当的
现在我们将考虑表示集合，它的表示就不那么直观了，可能有多种可能的表现方式

1. 无序列表
2. 有序列表
3. 二叉树
...

当然，即使它的实际表示方式可能是千变万化的，但是我们定义描述集合这个抽象数据的方式依然可以使用 data abstract 的方式：
即定义 constructor, selector 或者 predicate
