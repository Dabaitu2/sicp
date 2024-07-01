# Ways to achieve logic query system

1. Implement it as a nondeterminstic program, using the amb evaluator.
2. Manage with the search with the aid of streams.

## Central Operations: Pattern Matching and Unification

### Pattern Matching
A *pattern matcher* is a program that tests whether some datum fits a specified pattern. 

> `((a b) c (a b))` matches the pattern `(?x c ?x)`
> It also matches `(?x ?y ?x)` and `((?x ?y) c (?x ?y))`

The pattern matcher used by the query system takes as inputs a pattern, a datum, and a *frame* that specifies bindings for various pattern variables. 

> 也就是说这个 matcher 本身包含了一个 pattern，一个传入 pattern 的数据，和这些 pattern 中可能已经被部分赋值的那些变量的绑定（来自于之前的 pattern match)。 如果这个阶段的 datum 也可以匹配到 pattern, 就用 datum 中的各个部分匹配那个 frame 对应的 binding 进行填充。最后返回一个 frame。如果匹配失败，则直接标记失败。

For example,

1. using the pattern `(?x ?y ?x)` to match `(a b a)` given an empty *frame* will return a frame specifying that `?x` is bound to `a` and `?y` is bound to `b`.

2. Trying the match with the same pattern, the same datum, and a frame specifying that `?y` is bound to `a`  will fail.
3. Trying the match with the same pattern, the same datum, and a frame in which `?y` is bound to `b` and `?x` is unbound will return the gvien frame augumented by a binding of `?x` to `a`.



The pattern matcher is all the mechanism that is needed to process simple queries that don't involve rules.

`(job ?x (computer program))`

We scan through all assertions  in the *database* and select those that match the pattern with respect to an initially empty frame.

For each match the find, we use the frame returned by the match to instantiate the pattern with a value for `?x`.

### Streams of frames

We use stream to manage our output frame.

Let's say we have started to conduct a search procedure: 

We provide a empty frame.

We search all the entry of database, the result is either a special symbol indicating that the match has failed or an extension to the frame.

The result is a stream of all the frames that extend the given frame via a match to some assertion in the database.

![image-20240429151255156](../../../images/image-20240429151255156.png)

In our system, a query takes an input stream of frames and performs the above matching operation for every frame in the stream. And for each checking of the assertion we will generate a new stream. Which indicate the extension of existing frame by apply all sensible variables on unassigned varibles.

**To answer a simple query**, we use the query with an input stream consisiting of single empty frame. The resulting output **stream** of copies of the original query pattern with the variables instantiated by the values in each frame, and this is the stream that is finaly printed.



### Compound Queries

The processing of compound queries makes use of the ability of our matcher to demand that a match be consistent with a specified frame. 

复合查询的处理利用了我们的匹配器（matcher）要求 (demand) 匹配结果 (a match) 与指定的框架（a specified frame) 保持一致 (be consistent with) 的能力。

For example, to handle the **and** of two queries, such as

```scheme
(and (can-do-job ?x (computer programmer trainee)) 
     (job ?person ?x))
```

we first find all entries that match the pattern

```scheme
(can-do-job ?x (computer programmer trainee))
```

Thiis produces a stream of frames, each of which contains a binding for

?x. Then for each frame in the stream we find all entries that match

```scheme
(job ?person ?x)
```

in a way that is consistent with the given binding for `?x`. 

Each such match will produce a frame containing bindings for `?x` and `?person`.

#### And

The **and** of two queries can be viewed as a series combination of the two component queries.

>  下图展现了我们的 And 是如何作用于一个 compound query 的，我们将其拆分为两个 component query (有可能是 simple query, 也有可能是 compound query，甚至可能是 rule )按序执行
>
> 每一个 component query 都接受一个 stream of frames 和 data base 的数据，产出一个新的 stream frame，frame 中的每个元素都是对于这个查询执行后的一个变量的 binding . 而在下图中 B 接受的 stream of frames 则来自于 A，经过 B 和 database 后，产出一个新的 stream of frames，frame 中的 bindings 则同时包含了 `?x` 和 `?person`.

<img src="/Users/bytedance/Learning/CS/sicp/images/image-20240430163005533.png" alt="image-20240430163005533" style="zoom:50%;" />

#### Or

For **or** operation，it works as a parallel combination of two component queries. The input stream of frames is extended separately by each query. The two resulting streams are then merged to produce the final output stream.

<img src="/Users/bytedance/Learning/CS/sicp/images/image-20240430163910194.png" alt="image-20240430163910194" style="zoom:50%;" />

It's obvious that the execution of complex queries can be time-costing because it's like a tree shape, which in worst case can lead to exponential in the number of queries.

#### Not

From the stream-of-frames viewpoint, the **not** of some query acts as a filter that removes all frames for which the query can be satisfied. For instance, given the pattern

```scheme
(not (job ?x (computer programmer)))
```

we attempt, for each frame in the input stream, to produce extension frames that satisfy `(job ?x (computer programmer))`. We **remove** from the **input stream** all frames for which **such extensions exist.** 

For example, in processing the query

```scheme
(and (supervisor ?x ?y)
		 (not (job ?x (computer programmer))))
```

the first clause will generate frames with bindings for ?x and ?y. the **not** clause will then filter these by removing all frames in which the binding for ?x satisfies the restriction that ?x is a computer programmer.

> 对于 not 来说
>
> 1. 首先通过上游获取了 input stream of frames , 每个 frame 都是包括 ?x 和 ?y 的 bindings
>
> 2. 然后通过 (job ?x ...) 找到那些 ?x 是 computer programmer 的 stream of frames. 每个 frame 里面的 binding 就是 ?x
>
> 3. 然后将 1 中的 stream 进行 filter (参见 add, 本质上就是按序执行)，2 中返回的 stream 中的元素会被排除(求差集)。

#### lisp-value

The lisp-value special form is implemented as a similar filter on frame streams. 

We use each frame in the stream to instantiate any variables in the pattern, then apply the Lisp predicate. We remove from the input stream all frames for which the predicate fails.

### Unification

In order to handle **rules** in the query language, we must be able to find the rules whose **conclusions match a given query pattern.** 

Rule **conclusions** are like **assertions** except that they can contain variables, 

```
(rule ⟨conclusion⟩ ⟨body⟩) 
where ⟨conclusion⟩ is a pattern and ⟨body⟩ is any query

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))
```

so we will need a generalization of pattern matching—called **unification**—in which both the “pattern” and the “datum” may contain variables.

> 为了在查询语言中处理**规则**，我们必须能够找到**结论与给定查询模式相匹配的规则。**
>
> **规则的结论**类似于**断言**，但是它们可以包含变量，因此我们需要模式匹配的一种泛化——称为**合一**——其中"模式"和"数据"都可以包含变量。
>
> 详细来说
>
> 1. 我们在使用 Rule 的时候，第一步是要找到我们定义 Rule 的地方，并且判断我们定义的 Rule 和使用 Rule 应该是一个（类比于通过函数名寻找函数签名, 假设函数签名匹配不上说明当前这个函数签名并不是我们需要的)。
> 2. 相较于函数签名的名字和入参，规则使用 Conclusion 这一名词来类比。
> 3. Conclusion 长得像一个断言 assertion，但是它可以包含变量。
> 4. 我们在通过某个语句去查询可能的 data 或者 rule 时，需要有一个通用的方法，既可以处理这个语句本身对应就是一个对 database 的查询，又可以处理这个语句是对应一个 Rule 的结论的匹配。
>    1. 对于前者，正如前面所说，我们会利用一般的模式匹配找到 database 中符合模式的数据完成。
>    2. 而对于后者，我们则不能直接去把它用一般的模式匹配进行数据的检索了，而是要保留这个变量，去系统中找到我们可能定义的符合这种变量的 Rule 的 conclusion.
>
> 5. 因此，我们就引入了 Unification，即合一，合一是模式匹配的泛化（更宽泛的模式匹配），因为除了模式，它匹配的数据都是可以包含**变量**。这有别于一般的模式匹配。

A unifier **takes two patterns**, each containing constants and variables, and determines whether it is possible to assign values to the variables that will make the two patterns equal. If so, it returns a frame containing these bindings.

For example, unifying `(?x a ?y)` and `(?y ?z a)` will specify a frame in which `?x` , `?y` and `?z` must all be bound to `a`.

On the other hand, unifying `(?x ?y a)` and `(?x b ?y)` will fail cuz there's no value for `y` that can make the two patterns equal.

> 作为一种更加泛化的模式匹配，Unification 不是利用 datum 去匹配 pattern，而是检测两个 pattern (因为其中带有变量，所以本身就是 pattern 而非纯数据了)是否可以通过某种方式“合一”。也就是如果这个 pattern 中的 bindings 可不可以在全部或部分赋值后表示同样一个 pattern. 
>
> 实现 合一 的算法是查询系统中最困难的部分，在面对复杂 pattern 的时候，就像是在解一个联立方程组。不过，和解方程组不同之处在于我们并不需要去求解所有的 未知数 => 将所有的 variable 进行绑定，而是可以在部分 variable 依然处于未绑定的情况下完成匹配。
>
> 合一的结果如果成功会产生一个 frame，frame 中自然也是包含了满足这个合一条件的那些变量的 bindings. 如果失败，则直接会报错。

### Applying rules

Consider processing a query that involves applying a rule, such as 

```scheme
(lives-near ?x (Hacker Alyssa P))
```

To process this query,

1. We use the **ordinary pattern-match** procedure describle above to see if there are any assertions in the data base that match the pattern.
2. Attempt to **unify** the query pattern with the conclusion of each rule.

> 这里给出了一个涉及到 applying rule 的实例，
>
> 假设我们有上面一个这样的调用，现在，我们还不知道这样的一个调用是一个简单查询(Simple Query) 还是一个被抽象过的 rule, 因此
>
> 在我们理想的查询语言中，首先将当前语句看成简单查询。使用通常的模式匹配直接去寻找是否有一个 datum 。(这里命名为**断言 assertion,** 因为是一个逻辑查询语言，一个 assertion 定义了一个为真的描述条件). 
>
> 接着，我们会尝试将当前语句看成 Rule，通过 “合一” 去尝试找到这个 Rule 调用对应的 Conclusion 段。这里就需要将构成 conclusion 的 pattern 和当前的 pattern 进行合一。

We find that the pattern unifies with the conclusion of the rule

```scheme
(rule (lives-near ?person-1 ?person-2)
			(and (address ?person-1 (?town . ?rest-1)) 
     			 (address ?person-2 (?town . ?rest-2))
     			 (not (same ?person-1 ?person-2))))
```

resulting in a frame that `?person-2` is bound to `(Hacker Alyssa P)` and that `?x` should be bound to `?person-1`.

> 以上面的 Rule 为例子，我们将前面的调用和这里 Rule 的 conclustion 段进行合一，结果成功，并且能够产出一个 frame，其中 `?person-2` 会被绑定到 `(Hacker Alyssa P)` 上，`?x` 会被绑定到 `?person-1`  上 (这像是一种间接引用).

Now, relative to this frame, we evaluate the compound query given by the body of the rule, successful matches will extend this frame by providing a bindng for `?person-1`, and consequently a value for `?x`, which we can use to instantiate the original query pattern.

> 基于合一所产出的 frame, 我们以此作为基础求值 rule 的 body，也就是一个复合查询 (Compound Query). 这里的求值方式就如同上文所言，不过初始的 frame 则不再是空的。
>
> 求值目标复合查询如果成功，则 `?person-1` 也应该被成功的绑定, 那么通过引用传递，我们自然也能够获得 `?x` 的值。
>
> 最后，我们就利用这个被扩展完毕的 frame (的 stream?) 去 instantiate 原始的 query pattern (也就是把变量替换成真实的 datum)
>
> 通过上面的两个部分，总结一下实际执行 Rule 的过程
>
> 1. 通过将查询和 Rule 进行 Unify，如果成功，则产生一个基于原始 frame 的扩展
> 2. 基于这个扩展 frame，求值这个 Rule 的 body 所构成的复合/简单查询。
>
> 我们可以发现这和我们实现 lisp 的通用 eval/apply 过程非常类似：
>
> 1. 将过程的形式参数绑定到其传入的实际参数对应的数据上，从而形成一个 frame，这个 frame 扩展了原始的程序执行环境。
>
> 2. 根据这个被扩展的环境，解析过程中 body 对应的表达式。
>
>    ```scheme
>    (define (apply procedure arguments)
>      (cond
>        [(primitive-procedure? procedure)
>         (apply-primitive-procedure procedure arguments)]
>        [(compound-procedure? procedure)
>         (eval-sequence (procedure-body procedure)
>                        (extend-environment
>                         (procedure-parameters procedure)
>                         arguments
>                         (procedure-environment procedure)))]
>        [else
>         error
>         "Unknown procedure type -- APPLY"
>         procedure]))
>    ```
>
>    

### Simple queries

We saw eariler in this secion how to evaluate simple queries in the absense of rules (By pattern matching with stream of frame).

Now that we have seen how to apply rules, we can describe how to evaluate simple queries by using both rules and assertions.

Given the query pattern and a stream of frames, we produce,  for each frame in the input stream, two streams:

- a stream of extened frames obtained by matching the pattern against all assertions in the database (Using the pattern matcher).
- a stream of extend of extended frames obtained by appluying all possible rules (using the unifier).

Appending these two streams produces a stream that consists of all the ways that the given pattern can be satisfied consistent with the original frame.

> 假设我们的查询提供了一个起始的 stream，其中的每一个 frame 都会产出两个 stream。一个是通过通用的模式匹配获取到的所有 bindings，另一个是通过 Unification 获取到的所有 bindings.
>
> （这里需要说明的是，所有的简单查询一开始都是没有 input stream 的，而我们的求值过程会初步将模式和数据库中的断言进行匹配，并产生初步的 input stream，再基于这个 input stream 去扩展。）
>
> 然后我们将上面形成的所有 bindings 进行 append，最后会得到一个 large stream。
>
> 例如，假设假设我们的模式是 (f ?x ?y),输入流中有两个 frame ,绑定分别为 (?x . 1), (?x . 2)。为了找到所有的匹配,我们需要将 ?y 绑定到所有可能的值,每一种绑定都会生成一个新的帧, 假设我们通过模式匹配和 unification 分别完成了一个扩展，那么就形成了两个流, 例如
>
> ```scheme, 
> ;; for (?x. 1)
> (stream (table (?x . 1)  (?y . 2)), ..)
> (stream (table (?x . 1) (?y . 3)), ..)
> 
> ;; for (?x . 2)
> (stream (table (?x . 2)  (?y . 4)), ..)
> (stream (table (?x . 2) (?y . 5)), ..)
> ```
>
> 之类，然后他们分别进行合并， 各自获得了基于初始 input stream 的所有可能扩展 流
>
> ```scheme
> (stream (table (?x . 1)  (?y . 2)), 
>         (table (?x . 1) (?y . 3))),
> 				...)
> (stream (table (?x . 2)  (?y . 4)),
>         (table (?x . 2) (?y . 5))
>         ...)
> ```
>
> 最后再将其做最后一次合并， 形成这样的一个 large stream， 就包含了我们所有符合基于给定 pattern 的初始输入流的扩展 frame 的情形。
>
> ```scheme
> (stream (table (?x . 1)  (?y . 2)), 
>         (table (?x . 1) (?y . 3))),
> 				(table (?x . 2) (?y . 4)),
>         (table (?x . 2) (?y . 5))
>         ...)
> ```

### The query evaluator and the driver loop

Despite the complexity of the underlying matching operations, the systerm is organized much like an evaluator for any language. The procedure that coordinates the matching operations is called `qeval`, and it plays a role analogous to that of the `eval` procedure for lisp.

`qeval` takes as inputs a query and a stream of frames.

Its output is a stream of frames, corresponding to successful matches to the query pattern, that extend some frame in the input stream.

Like eval, `qeval` classifies the different types of expressions (queries) and dispatches to an appropriate procedure for each. There is a procedure for each special form (and, or, not, and lisp-value) and one for simple queries.

> 查询语言的 eval 和一般的 eval 比较类似，命名为 qeval，
>
> 其入参是一个 query 查询和一个 frame 的流。出参也是一个 frame 的流。出参数中的 frame 就是针对入参 frame 的扩展形成。
>
> 和通用的 eval 一样，qeval 也是针对不同类型的表达式（规则）派发给不同的过程去处理。

The driver loop, which is analogous to the driver-loop procedure for the other evaluators in this chapter, reads queries from the terminal.

**For each query, it calls qeval with the query and a stream that consists of a single empty frame.**

This will produce the stream of all possible matches (all possible extensions to the empty frame). 

For each frame in the resulting stream, it instantiates the original query using the values of the variables found in the frame. This stream of instantiated queries is then printed.

> 最终，我们的查询语言的交互系统 driver-loop 和一般的 evaluator的 driver-loop 比较类似，都是从终端读取用户输入。
>
> 对于我们的 driver-loop 而言，它读取 query 后，通过执行 qeaval ，将当前 query 和一个包含单个 空 frame 的 stream 传入。获得结果 stream
>
> 然后再 stream 中的每个 frame 中，都用其中的 binding 去实例化原有的 query 形成一个新的 frame，最后将这个 frame stream 打印出来。

The driver also checks for the special command assert!, which sig- nals that the input is not a query but rather an assertion or rule to be added to the data base. For instance,

```scheme
(assert! (job (Bitdiddle Ben)
              (computer wizard)))
(assert! (rule (wheel ?person)
				 (and (supervisor ?middle-manager ?person)
              (supervisor ?x ?middle-manager))))
```

> 最后，driver 还有能够识别用户 “插入数据” 的操作，这通过 assert! 来标识它不是一个 query。
