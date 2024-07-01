#lang sicp

;; We can regard Rule as a kind of logical implications (逻辑蕴含/包含):
;; *If* an assignment of values to pattern variables satisfied the body,
;; *Then* it satisfies the conclusion.
;; P.S
;; Logical Implications 是一个逻辑学概念，表明两个陈述或句子存在的关系，这种关系转成口头语言叫做
;; 逻辑上包含 或者 如果就(if/then), 这个符号由指向右边的双箭头表示 (=> )
;; 例如
;; 逻辑包含的一个例子是假设句子A 和B 由下面指定：A=天空是多云的。B=看不见太阳。
;; A => B 则对应这样一个逻辑蕴含: 如果天空是多云的，那么看不见太阳。

;; 因此我们可以认为我们的查询语言具有基于某种规则执行逻辑推理的能力.

;; 以我们在前面提到的 append 作为例子
;; 1. 对于任何一个 list `y`, 对 empty list 和 `y` 作 `append` 形成的就是 `y`
;; 2. 对任意的 `u`, `v`, `y`, `z` , 如果 `v` 与`y` 作 append 可以形成 `z` , 则 将 `(cons u v)` 与 `y` 作 `append` 将形成 `(cons u z)`

;; 对应写出的规则如下:
;; 其中第一条代表 ?y 都满足条件, 标识结论对任何值都成立
;;     第二条通过递归的形式表示了结论的推导过程, 首先通过 (?u . ?v) ?y (?u . ?z) 去捕获所有满足基本形式的变量
;;     然后通过将这些变量传给递归的自己去尝试判断是否符合，递归推导有可能再次进入规则2，可能进入规则 1, 规则 1 其实就是基准条件
(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

;; 理论上会得到这样的答案
;;; Query input:
(append-to-form (a b) (c d) ?z)
;;; Query results:
(append-to-form (a b) (c d) (a b c d))

;;; Query input:
(append-to-form (a b) ?y (a b c d))
;;; Query results:
(append-to-form (a b) (c d) (a b c d))

;;; Query input:
(append-to-form ?x ?y (a b c d))
;;; Query results:
(append-to-form () (a b c d) (a b c d))
(append-to-form (a) (b c d) (a b c d))
(append-to-form (a b) (c d) (a b c d))
(append-to-form (a b c) (d) (a b c d))
(append-to-form (a b c d) () (a b c d))
