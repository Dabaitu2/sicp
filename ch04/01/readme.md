# 4.1 The Metacircular Evaluator

> 元循环求值器
>
> 用与被求值 的语言同样的语 言写出的 求值器被称为元循环 Metacircular 。



## 求值器的基本思路

### 基本构造: Eval & Apply

元循环求值器本质上就是 3.2 节描述的环境模型的 scheme 实现
该模型包括两部分：

1. 求值一个组合表达式时，首先求解其中各自表达式，然后将值应用于外界表达式, 例如 `(* (+ 1 2) (- 4 2))` 中，先求解内部再求解外部
2. 将一序列实参应用于一个复合过程时，在一个新的环境中去求值这个过程体，而我们会使用一个 frame 去构造这个环境，并且在这个 frame 中存放形式参数和实际参数值的绑定

这两个规则描述了求值过程的本质: 一个基本循环

在这个循环中，表达式在环境中求值被归约到将实际参数应用到过程上。而这一步又反过来归约到新的表达式在新的环境中求值，如此下去直到我们下降到遇到 symbols 也就是那些可以在环境中找到 binding 的变量/值，或者基本过程 `(+ 1 2)` 这种可以被直接应用而无需进一步归约的过程。

这个求值循环会由两个关键的过程相互作用形成：`eval` 和 `apply`

<img src="/Users/bytedance/Learning/CS/sicp/images/image-20240718190230909.png" alt="image-20240718190230909" style="zoom:50%;" />

> 这样的循环其实就是:
> 将过程 apply 给参数 <=> **eval**uate 表达式

```scheme
;; 数据导向的 eval
;; 特殊 form 都是一个 (cons tag exp)
;; 这类特殊 form 的表达式解析器都用 put 'exp 注册到 table 里面
(define (eval exp env)
  (cond
    [(self-evaluating? exp) exp]
    [(variable? exp) (lookup-variable-value exp env)]
    ;; 如果可以找到注册的特殊表达式，就执行其 eval 逻辑
    [(get 'exp (car exp)) ((get 'exp (car exp)) exp env)]
    ;; application 不是 (cons tag xxx) 的特殊表达式, 需要单独处理
    [(application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env))]
    [else (error "Unknown expression type: EVAL" exp)]))


(define (apply procedure arguments)
  (cond
    [(primitive-procedure? procedure)
     (apply-primitive-procedure procedure arguments)]
    [(compound-procedure? procedure)
     (eval-sequence (procedure-body procedure)
                    (extend-environment
                     (procedure-parameters procedure)
                     arguments
                     (procedure-environment procedure)))]
    [else
     error
     "Unknown procedure type -- APPLY"
     procedure]))

```





### 利用定义表达式的语法 (Syntax) 实现表达式解析

我们对于求值器的实现，依赖于一些定义了被求值的表达式的语法形式的过程。例如我们要定义什么情况是赋值，什么情况是加减乘除,  什么情况是 if / cond 等。

我们的实现是语言无关的。因此我们不是通过去检查某一个 表达式是不是以 `set!` 开头来决定其是不是赋值，而是通过一个通用的 predicate 过程 `assignment?` 去判断，同样的，我们使用 `assignment-variable` 和 `assignment-value` 去访问赋值中的相关部分

同时，我们还需要定义一些刻画过程和环境表示的操作。例如：`make-procedure` 会构造一个复合过程 `lookup-variable-value` 获取变量的值，`apply-primitive-procedure` 会将基本过程应用给一组实际参数.

很明显, 针对每一个语法形式, 我们都可以按照第二章中的数据抽象方案, 去实现它的 selector 和 constructor. 以及可能使用的 predicate.

```scheme
;; 处理 if 表达式
(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
;; 这里返回的 'false, 我们会在全局环境中为其建立一个 binding?
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) (cadddr exp) 'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
```

我们涉及的表达式包括:

1. 最基础的表达式:  self-evaluate 和 variable
2. 基本的特殊形式, 也就是语法: if / define / set! / lambda 等等. 
3. 由基本的特殊形式衍生的派生形式: cond / and / or / let(是 lambda 的语法糖) 等
4. 过程 procedure, 包括底层提供的 primitive procedure 和用户提供的 compound procedure. 



### 求值器内部所需的数据结构

为了保证我们的求值器能够正确的运行, 我们需要定义一系列的内部使用的数据结构来协助. 包括:

1. 一些内部 predicate 
2. 用于表示过程的数据结构以及用来“执行” 某个过程的方法, 本质上就是回到 eval 按照语法获得表达式结果, 并视情况扩展到 env 中.
3. 用于操作环境的数据结构和方法.包括扩展环境(增加 frame 和 binding, 修改 binding 等)



### 语法分析和执行分离

原始的 eval 实现，解析语法结构 和执行过程 是交织的, 比如下面这个例子

```scheme
(define (factorial n)
  (if (= n 1) 1 (* (factorial (- n 1)) n)))
```



在 evaluator 中，我们在 define 这个过程时，只是将函数体存了起来, 在后续我们频繁调用 `(factorial x)` 时，每调用一次这里面的函数体都需要被重新 eval
而 eval 中实际包含着解析语法结构的过程.

比如，我们分析 factoral 的 body 取出其 predicate, consequence 和 alternate 就是在做语法分析 (也就是调用 if-predicate / ... 这些命令), 我们通过 if 去判断当前 factorial 的 body 到底是个什么表达式也是在做语法分析,  我们现在每调用一次就得搞一次.

假设，我们在 (define xx) 的时候，就预先解析了这个 xx 的结构, 后续调用 xx 时，如果直接获取这个结构，就省的一次次的再分析了

一般来说，我们似乎可以存一个全局的变量映射去关联解析的结果和被解析的表达式. 不过本章节给出了个更优雅的策略：**使用函数去封装**. 也就是说，我们的解析 xx 时，返回的是一个 lambda，其中封装了后面要做操作的概念, 这样，每当我们从环境中取某个变量对应的结果时，比如这个变量对应的本来是个过程. 而在这里，就是一个被解析完成的过程(包裹在 lambda 内), 我们只需要将当前的 env 传进去，就可以快速的执行结果,  省去了多余的语法分析步骤

这在现代的编程语言中非常常见，是一种类似 JIT 分析的东西

因此我们设计一个 analyze 方法来实现这个能力, 它接收一个表达式，返回一个 过程，称为 execution procedure (执行过程),  execution procedure 封装了在后续执行被解析的表达式时将要完成的工作, 我们再把 env 传给这个 执行过程完成最后的执行操作

```scheme
(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond
    [(self-evaluating? exp) (analyze-self-evaluating exp)]
    [(variable? exp) (analyze-variable exp)]
    [(get 'exp (car exp)) ((get 'exp (car exp)) exp)]
    [(application? exp) (analyze-application exp)]
    [else
     (error "Unknown expression type -- ANALYZE" exp)]))
```



对于其他的具体实现，已经融入了 evaluator-analyzer 文件夹中. 这里不再单独写出
