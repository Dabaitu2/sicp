# 4.3 Variations on a Scheme — Nondeterministic Computing

> Scheme 的变体 - 非确定性计算

## Prelude: 非确定性计算的描述和原理

下面代码给出的就是一种被称为 non-determistic computing 非确定性计算的描述

之所以叫非确定性计算，是因为它没有给出应该如何真正的具体实现求解的方案, 而是给出了类似对于定义说明的翻译，或者说偏 pseudo 代码的东西 (是什么 what, 而非如何做 How)

本章的目标，就是要通过修改求值器以实现此能力. 

```scheme
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (prime-sum-pair list1 list2)
  (let ([a (an-element-of list1)]
        [b (an-element-of list2)])
    (require (prime? (+ a b)))
    (list a b)))
```



此处依赖的关键 idea 是:  

在一个非确定性语言中，表达式可以有多于一个可能的值，例如 an-element-of 可能返回给定的 list 中的任何一个元素, 我们的求值器会自动从中选出一个可能值,  并且跟踪这个选择, 如果随后的 require 无法满足，求值器就会尝试其他的选择直到成功或者选择已经用光. 



### 对比非确定性求值与流处理

比较 非确定性求值 和 流处理 是很有指导意义的

流处理中利用了**惰性求值**，设法将流处理使用惰性求值来将可能答案流的**组装时间**与实际流元素的**生成时间**解耦, 在这种求值其中，我们构造出一种幻觉，所有可能结果都以一种**和时间无关的方式**摆在面前。

> 流即答案, 流即全部. 在结果实际生成前, 我们也认为结果存在了.

而非确定性求值, 表达式则代表着对一系列的可能世界的探索。每一个世界都是由一系列选择所决定的。一些世界可能会走入 dead end，但另一些则可能会保存着有用的值。 

在这种求值中，我们会构造出另一种幻觉（假象 illusion):  **时间存在分支**

我们的程序中保存着所有可能的不同执行历史，再遇到一个 dead end 时，我们总可以回到以前的某个选择点, 并沿着另一个分支继续下去



## 4.3.1 Amb 和搜索

在本章节中, 我们想要实现的这个求值器称为 **amb evaluator (ambiguous evaluator)**

它具有如下的表现

```scheme
;;; Amb-Eval input:
(prime-sum-pair '(1 3 5 8) '(20 35 110)) ;;; Starting a new problem
;;; Amb-Eval value:
(3 20)
```

`(amb ⟨e1⟩ ⟨e2⟩ . . . ⟨en⟩)` 会**不明确的**返回 e1..en 中的某一个值 ei, 而再之后如果调用 (amb) 代表 "计算失败". 我们可以将 amb, 这意味着这个分支失败了 因为amb 没有选择到任何的一个值, 那么求值器将再推一个数据
例如上面这段代码可能会产生出如下 6 个可能的值
`(1 a) (1 b) (2 a) (2 b) (3 a) (3 b)`.

本质上，amb 所做的事情就是在搜索: 

一个 amb 代表了一个  nondeterministic choice point (非确定性选择点) 那么对于 amb 有两种使用方法：

1. 假设我们有无穷的机器，无穷的并发能力，每当遇到 amb，我们都会分配更多的机器去处理可能产生的值，或者遭遇成功/结束

2. 如果 (大部分情况下) 我们的只有一台机器，只能执行一个或有限个并发进程, 我们就必须实现成**顺序的方式**。是指再遇到一个选择点的时候随机选取分支继续。而一旦这样的搜索遇到了失败, 我们就必须重新运行求值器（不是 amb，是整个求值器)，再随机的选择. 但这样肯定就是肉眼可见的低效了。因此我们可能需要一种系统化的搜索所有的可能执行路径。

我们本节要实现的系统化搜索方式，就是

1. 每次遇到 (amb) 时，默认选择第一个可能性。
2. 一旦遇到失败，求值器可以自动 magically 回溯到最近的选择点去实验下一个可能性
3.  如果在任何选择点用完了所有可能性，就退出到上一层选择点，从那里继续

这种策略称为 depth-first-search 或者 chronological backtracking （按时间回溯)



## 4.3.2 非确定性程序的实例

在这里我们会利用几个实例来展示非确定性计算的威力, 在不需要实现具体计算细节的基础上就可以实现对问题的求解

### Logic Puzzles 逻辑谜题

> Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an apartment house that contains only five floors.                                                                            
>
> Baker does not live on the top floor.
> Cooper does not live on the bottom floor.
> Fletcher does not live on either the top or the bottom floor.
> Miller lives on a higher floor than does Cooper.
> Smith does not live on a floor adja- cent to Fletcher’s.
> Fletcher does not live on a floor adjacent to Cooper’s.
>                                                                                
> Where does everyone live?

通过编写如下的代码, 我们就可以解决这个问题: 本质上, 我们只需要给出所有可能选项, 再通过 require 施加层层约束就可以解决此问题. 本质上, 就是将可能需要层层书写的循环/递归交给了解释器实现.

```scheme
(define (require p)
  (if (not p) (amb)))

;; 判断是否均是独立 item
(define (distinct? items)
  (cond
    [(null? items) true]
    [(null? (cdr items)) true]
    [(member (car items) (cdr items)) false]
    [else (distinct? (cdr items))]))

;; 一个比较慢的方法, 简单枚举所有可能性并加上约束
;; 习题 4.39/40 讨论了一些改进
(define (multiple-dwelling)
  (let ([baker (amb 1 2 3 4 5)]
        [cooper (amb 1 2 3 4 5)]
        [fletcher (amb 1 2 3 4 5)]
        [miller (amb 1 2 3 4 5)]
        [smith (amb 1 2 3 4 5)])
    (require (distinct?
              (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; 仅有 1 个解
(define stime (runtime))
(multiple-dwelling)
(display (list "Time Taken: " (- (runtime) stime)))
```

### 自然语言解析

解析输入的句子，解析其中的语法部分

假设我们有一个简单的句子: **The cat eats** , 它理论上就可以分析为：

`(sentence (noun-phrase (article the) (noun cat)) (verb eats))`

代表这个句子由 冠词 the, 名词 cat 组成的**名词短语** 和 动词 eats 组成. 

分析这个句子实际上是要产生 sentence 这个结构, 这个结构分别由两个部分构成.

`(parse-noun-phrase)` 和  `(parse-word)`, 会分别被调用从而求得这两个部分

而 (parse-noun-phrase) 又是由 article 和 noun 构成的.

```scheme
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))


(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ([found-word (car *unparsed*)])
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ([sent (parse-sentence)])
    (require (null? *unparsed*))
    sent))

(parse '(the cat eats))
;;; (sentence (noun-phrase (article the) (noun cat)) (verb eats))

```

到目前为止，amb 的主要作用是能够构成 require 从而方便的描述约束条件
但如果我们的句子结构变得更加复杂，它就可以处理更多内容, 因为我们往往需要进行 "回溯"
去尝试另一种句法结构判断. 

例如, 对于一个动词短语, 它可以是一个简单的动词 

`('verb xxx)` => we [love] money

也可以是一个动词短语 + 一个介词短语 

`('verb-phrase verb-phrase prepositional-phrase)` => we [wait for the cat] / we [fight for the future]

不过，我们可以想见，如果句子合法，最终动词短语的第一个被解析的单词将一定是一个简单动词，不然递归不会终止

在这里，amb 就会发挥"回溯"的作用, 当我们遇到一个 verb 的时候，我们可能会检测它是否能够构成 verb + 介词短语, 还是只是一个简单动词，这就通过 amb 来实现.

```scheme
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (define v-answer
      ;; 要么是一个简单动词
      (amb verb-phrase
           ;; 要么是动词短语 + 介词短语
           (maybe-extend
            (list 'verb-phrase
                  verb-phrase
                  (parse-prepositional-phrase)))))
     v-answer)
  (maybe-extend (parse-word verbs)))
```



## 4.3.3 实现 amb

参见 [Amb求值器](./03-implementing-amb-evaluator/readme.md)
