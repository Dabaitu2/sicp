#lang sicp

;; 原始的 eval 实现，解析语法结构 和执行过程 是交织的
;; 比如下面这个例子

(define (factorial n)
  (if (= n 1) 1 (* (factorial (- n 1)) n)))

;; 在 evaluator 中，我们在 define 这个过程时，只是将函数体存了起来
;; 在后续我们频繁调用 (factorial x) 时，每调用一次这里面的函数体都需要被重新 eval
;; 而 eval 中实际包含着解析语法结构的过程，比如，我们分析 factoral 的 body
;; 取出其 predicate, consequence 和 alternate 就是在做语法分析 (也就是调用 if-predicate / ... 这些命令)
;; 我们通过 if 去判断当前 factorial 的 body 到底是个什么表达式也是在做语法分析

;; 我们现在每调用一次就得搞一次
;; 假设，我们在 (define xx) 的时候，就预先解析了这个 xx 的结构
;; 后续调用 xx 时，如果直接获取这个结构，就省的一次次的再分析了
;; 一般来说，我们似乎可以存一个全局的变量映射去关联解析的结果和被解析的表达式
;; 不过本章节给出了个更优雅的策略：使用函数去封装
;; 也就是说，我们的解析 xx 时，返回的是一个 lambda，其中封装了后面要做操作的概念
;; 这样，每当我们从环境中取某个变量对应的结果时，比如这个变量对应的本来是个过程
;; 而在这里，就是一个被解析完成的过程(包裹在 lambda 内), 我们只需要将当前的 env 传进去，就可以快速的执行结果
;; 省去了多余的语法分析步骤

;; 这在现代的编程语言中非常常见，是一种类似 JIT 分析的东西

;; 因此我们设计一个 analyze 方法来实现这个能力, 它
;; 接收一个表达式，返回一个 过程，称为 execution procedure (执行过程)
;; execution procedure 封装了在后续执行被解析的表达式时将要完成的工作
;; 我们再把 env 传给这个 执行过程
;; 完成最后的执行操作
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

;; 对于其他的具体实现，已经融入了 evaluator-analyzer 文件夹中
;; 这里不再单独写出
