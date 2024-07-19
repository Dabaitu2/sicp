# Is Logic Programming Mathematical Logic?

The means of combination used in the query language may at first seem identical to the operations `and`, `or`, and `not` of mathematical logical, and the application of query-language rules is in fact accomplished through a legitimate method of inference.

> 我们在查询语言中使用的 组合 方式看起来和 逻辑数学中的 `and`, `not`, `or` 是一样的。并且逻辑语言中使用的 规则(rule) 的应用也是通过基于 推断的合理方法实现的。这里我们实际上使用的方式为 modus ponens 假言推理。
>
> 即：若 A 为真且 A=>B 也为真 (形式描述为A蕴含B为真)， 则我们认为 B 为真。说人话就是，如果我们已知一个推断（当A的时候就是B) 为真，若此时 A 也是真，那么 B 就一定也是真。比如，**人被杀就会死**是一个蕴含，此时，如果**一个人被杀**了，那么他肯定死了。
>
> 那么很明显，我们在定义 rule 的时候，包含两个东西，即 conclusion 和 body，而我们实现 application 和 rule 的对应关系也是：如果一个 assertion 满足了 rule 的 body，则它也满足 rule 的 conclusion，因为 rule 本身就是一种 **逻辑蕴含** 的定义，因此在 rule 的概念上，这是基于推断的合理的数理逻辑方法。

The identification of the query language with mathematical logic is not really valid, though, because the query language provides a `control structure` that interprets the logicalstatements procedurally. And we can offen take advantage of this control structure.

> 但实际上，我们的查询语言和数理逻辑不可以说是完全等同的。因为我们的查询语言提供了一种控制结构，采用过程性的方式来解释逻辑语句。并且在实践中这种控制结构会为我们带来一些性能上的好处。
>
> 例如下面的两个查询在逻辑上是完全等价的：
>
> ```scheme
> (and (job ?x (computer programmer)) (supervisor ?x ?y))
> (and (supervisor ?x ?y) (job ?x (computer programmer)))
> ```
>
> 但是，如果一个公司中如果上司比员工多，那么采用第一个查询在实践中会具有更优的性能。

The aim of logical programming is to provide the programmer with techniques for decomposing a computation problem into two seperate problems: "What" is to be computed, and "how" this should be computed.

This is accomplished by selecting a subset of the statements of mathematical logic that is powerful enough to be able to describe anything one might want to compute, yet weak enough to have a controllable procedural interpretation.

> 在一般的数值计算程序设计之外，为什么需要逻辑程序设计？
>
> 逻辑程序设计的目标是为程序员提供一种技术：将计算性问题分解成两个相互分离的问题：要计算什么(what)，以及如何进行计算(how)。正如我们在本章最开始所提到的，也许我们可以只去解决**计算什么**的问题，而如何进行计算的问题由逻辑程序设计语言来自动提供。
>
> 达成这个目标的一种方式就是找到数理逻辑语句中的子集：它足够强大可以描述所有可能希望去计算的问题。但又足够弱，从而能够被过程性的解释描述。

The intetntion here is that，on the one hand, a program specified in a logic programming language should be an effective program that can be carried out by a computer. Control (“how” to compute) is effected by using the order of evaluation of the language. We should be able to arrange the order of clauses and the order of subgoals within each clause so that the computation is done in an order deemed to be effective and efficient.  At the same time, we should be able to view the result of the computation (“what” to compute) as a simple consequence of the laws of logic.

> 之所以使用这种方式(找到一个既强又弱的子集)的意图（目的) 是：
>
> 在处理 计算什么 的问题的方面，我们的程序应该将计算的结果看作是逻辑定律的简单推论。这意味着，逻辑编程的结果应该符合逻辑规则，可以通过逻辑推理得出。换句话说，逻辑编程语言的语义应该建立在数理逻辑的基础上，程序的结果应该是逻辑推理的直接结果。
>
> 而在处理 如何计算 的问题的方面，逻辑编程语言中的程序应该是有效的，能够由计算机执行。我们应该能够安排子句的顺序和每个子句中子目标的顺序，以便以一种被认为有效和高效的顺序进行计算。这意味着，尽管逻辑编程语言基于数理逻辑，但我们仍然可以通过调整语句和目标的顺序来优化程序的执行效率。
>
> 总结来说，为了能够让我们的逻辑程序设计语言兼具数理逻辑的严谨性又具有编程语言的灵活性和可执行性，我们就需要去这样找一个子集。

Our query language can be regarded as just such a procedurally interpretable subset of mathematical logic. 

An assertion represents a simple fact (an atomic proposition). 

A rule represents the **implication** that the rule conclusion holds for those cases where the rule body holds.A rule has a natural procedural interpretation: To establish the conclusion of the rule, establish the body of the rule. Rules, therefore, specify computations. 

However, because rules can also be regarded as statements of mathematical logic, we can justify any “inference” accomplished by a logic program by asserting that the same result could be obtained by working entirely within mathematical logic.

> 那么，就我们将要实现的查询语言而言，它就可以被看作一个逻辑代数的过程性描述子集。在这个子集中
>
> 一个断言代表了一个基本事实(或者被称为原子命题)
>
> 一条规则代表了一个蕴含：所有使规则的 body 成立的情况，也使得规则 conclustion 成立 (body => conclusion). 而这也通过一个非常自然的过程性解释来完成：若要建立一个规则的结论，那么也需要建立规则 body (不过规则 body 可以是空的, 正如第2小节 的 same rule 描述的). 
>
> 同时，由于规则可以看作是数理逻辑的语句（它的 body 本身就是查询组成的), 那么我们即使没有规则，也可以完全使用数理逻辑来得到相同的结果。从而证明我们的程序建立起来的 推断 都是合理的（基于数理逻辑推断而产生)。(不过这个描述要加一个限制：计算是可以终止的, 而即使加了这个描述之后，我们设计的查询语言和 prolog 之类的逻辑程序语言也不能满足， 因为我们在程序中使用了 not 和 lisp-value, 这在下面会立马提到，为了使得我们的程序完全和数理逻辑等价，我们可以删除 not 和 lisp-value 这样的机制，只支持简单查询，但这样会极大地损害语言的表达能力，从而降低编写效率，找到表达能力和数理逻辑相容的良好方式也是当今逻辑程序设计研究中特别关注的一个方面)。

### Infinite Loop

A consequence of the procedural interpretation of logic programs is that it is possible to construct hopelessly inefficient programs for solving certain problems. An extreme case of inefficiency occurs when the sys- tem falls into infinite loops in making deductions. 

> 逻辑程序设计的过程性解释的一个可能后果就是可能写出非常低效的程序。一个极端的例子就是在推导时可能会陷入无限循环。

As a simple example, suppose we are seing up a data base of famous marriages, including

```scheme
(assert! (married Minnie Mickey))
```

If we now ask

```scheme
(married Mickey ?who)
```

we will get no response, because the system doesn’t know that if A is married to B, then B is married to A. So we assert the rule

```scheme
(assert! (rule (married ?x ?y) (married ?y ?x)))
```

and again query

```scheme
(married Mickey ?who)
```

which unfortunately will drive the system into an infinite loop.

> 上面的这个基于 married 关系的例子进行查询就会导致无限查询，因为常规的查询无法识别到 married A B 和 married B A 本质是一样的，因此我们需要定义一个规则 去说明这一点，可基于这个规则进行查询就会导致无限循环。
>
> 对于 (married Mickey ?who) 这个查询，我们在应用 rule 的 body 的时候，会再次遇到解析 (married ?y ?x), 首先我们直接寻找是否有这样的 datum 也就是 assertion，确实可以找到 (married Minnie Mickey)，然而，我们接下来还需要判断是否有 rule 也可以进行 unification. 这就会导致再次进入 rule 的 body 内部，从而触发无限循环。

### Problems with `not`

> 由于我们实现 not 的方式并不是 “逻辑性” 的，而是过程性的，这导致顺序可能对产出的结果实际上是有影响的。例如
>
> ```scheme
> (and (supervisor ?x ?y)
> 		 (not (job ?x (computer programmer))))
> (and (not (job ?x (computer programmer))) 
>      (supervisor ?x ?y))
> ```
>
> 对于第二个 Query，由于 not 的实现基础是基于从上面传入的 input stream 将其和 (job ?x (computer programmer)) 所构成的 stream 求差集，而在此例子上我们传入的 stream 是空的，这导致结果也是空的，并不能实际满足我们的需求，从而这个 not 在逻辑上显得是不完备的。
>
> 归根结底而言，这是因为我们将 not 解释成了一种 filter，这种问题同样也可能出现在  lisp-value 上面。在习题中, 通过引入 promise 我们可以一定程度上解决这个问题.
>
> 除此之外，还有一个更严重的和数理逻辑中的 `not` 出入的方面。我们在查询系统中的 not 并不是数理逻辑中的 “非 true”，而只是说结论无法由我们的数据库中的知识推算出来。从这看来，我们的逻辑程序设计也不可以说是和数理逻辑完全等价的。比如我们在 4.4.1 节建立的数据库可以推导一些各种各样的 not 语句，例如 Ben Birdiddle 不喜欢篮球，外面没有下雨等等，然而这其实是不合理的。



