# Amb 求值器

一般的求值器执行任何一个 scheme 表达式，返回只有三种情况
1. 返回一个值
2. 卡死在那，永远不终止
3. 抛出一个异常

而对于非确定性求值，情况就将变得更复杂



## Execution procedures and continuations

常规的求值器的执行过程中有一个参数：env 即执行环境

然而，对于 amb 求值器，我们需要三个参数: 执行环境，和两个被称为 `continuation procedure` 的过程。

表达式的求值结束后，将会被执行这两个 `continuation procedure` 中的一个：

1. 如果求值出一个结果，则调用 `success continuation` .
2. 如果遇到了死胡同，则调用 `failure continuation`



对于 **success continuation**, 它进行的工作是

1. 接受上面传进来的一个值，并继续计算。
2. 同时，这个 success continuation 会被传入另一个 **failure continuation**,  它会在使用 success continuation 接受值进行计算后产生 dead end 时被调用。

> 这里其实有点像是堆栈的恢复策略。
>
> 我们在执行一个个可能性测试的时候，就像是在向栈中保存当前层级现场。而一旦有一个测试失败，则
>
> 1. 尝试返回上一级，检查还有没有别的可能性，继续试探
> 2. 如果所有的可能性均已经试探结束，则再向上回溯。



对于 failure continuation, 它进行的工作是

试探 Non-deterministic Procedure 的另一种可能分支。

1. 求值器取出一种可能性，将他的值送给 success continuation 过程。
2. 求值器构造出一个新的 failure conitnuation, 也将其送给 succession continuation 过程。

> 这里的第二步就是实现 “回溯” 的关键，我们不是真的倒转历史了。而是将整个历史通过某种联系保留了。
>
> 我们在处理某个层级的求值时
>
> 1. 本质上它就是是上层 success continuation ，上层的 success continuation 本质上被解释器处理为下一步的调用。
> 2. 上层的 continuation 同时还传了一个东西来充当 failure conitnuation，而对于上层来说，本质上这个东西是**（试探下一个值)** 
> 3. 我们在还原副作用时，猜测是保存了原始值，将原始值重新 set 回去



在我们的求值过程中，标识当前的执行遇到 dead end 是通过 amb 表达式来实现的。

同时，如果我们的处理中存在副作用，副作用也应该被撤销。

##### Summary

对于 Failure continuation，它可以由如下情形构造

1. amb 表达式，用户主动触发，标志遇到 deadend
2. driver loop，提供一种机制，如果已经没有选择，则应该报告失败
3. 赋值：拦截失败并且撤销赋值效果

而反过来讲，当一个过程**失败时**，通常是因为：

1. 用户执行了 amb
2. 用户输入 try-again

Failure continuation 则会在处理失败的过程中被使用。



## Structure of Amb Evaluator

我们的求值器是基于 4.1 章节改造后的 analyze 版求值器(JIT版本) 实现的. 这里面需要改动的部分主要包括

### 增加 amb 相关的语法过程

由于 amb 求值器增加了 success continuation 和 failure continuation,  并将其得到的执行过程 (execution procedure: 被解析了语法结构的 body) 应用到给定的环境和 continuation 上.

一般的 analyze 后的语法结构的执行过程为: `(lambda (env) ...)`

而 amb 所使用的新的执行过程大概会变成:

```scheme
(lambda (env succeed fail)
   ;; succeed is (lambda (value fail) ...)
   ;; fail is (lambda () ..)
   ...
 )
```



因此, 我们的 eval 将会被改造如下: 简单来说就是传入了两个 continuation 协助进行时间回溯.

```diff
- (define (eval exp env)
-   ((analyze exp) env))

+ (define (ambeval exp env success fail)
+  ((analyze exp) env success fail))
```



同时, 我们也需要新增对 amb 这个特殊语法的支持:

```diff
(define (analyze exp)
  (cond
    [(self-evaluating? exp) (analyze-self-evaluating exp)]
    [(variable? exp) (analyze-variable exp)]
+   [(amb? exp) (analyze-amb exp)]
    [(get 'exp (car exp)) ((get 'exp (car exp)) exp)]
    [(application? exp) (analyze-application exp)]
    [else
     (error "Unknown expression type -- ANALYZE" exp)]))
```



而 amb 本身具有两个功能: 产出一系列值, 或者标志当前求值失败, 需要回溯, 我们在 analyze-amb 中需要照顾到这两点

```scheme
;; amb 的语法结构
;; (cons 'amb ...amb-choices)
(define (analyze-amb exp)
  ;; 解析所有 amb 的可能值
  (let ([cprocs (map analyze (amb-choices exp))])
    (lambda (env succeed fail)
      (define (try-next choices)
        ;; 如果已经没有选择了, 或者本身选择就是没有的, 那就直接调用上一级提供的 fail 去进一步回溯.
        (if (null? choices)
            (fail)
            ;; 尝试执行某一个可能值的求值
            ;; 这里 car 获得的东西肯定是一个被 analyze 过的东西，所以一定是个过程
            ((car choices)
             env
             ;; 在这里，只要 (choice env 没有什么幺蛾子(里面主动抛出 (amb))，应该是会成功的)
             succeed
             ;; 否则就调用 fail, 继续尝试下一个可能值
             ;; 考虑一个情况:
             ;; (let ((a (amb 1 2 3)))
             ;;   (if (= a 3)
             ;;       1
             ;;       (amb)))
             ;; 我们通常遇到的 (amb) 触发的失败, 假设前面有 (amb 1 2 3) 这样的产出值，为什么还能触发 try-next 呢？
             ;; 这样是因为 fail 会被传给 succeed 的第二个参数中，当这个 succeed 触发后续逻辑失败 (也就是 if 里的 amb)
             ;; 那么这个 fail 就会被调用, 从而触发 try-next, 这就是核心所在
             ;; 我们只需要考虑第一次的 amb 的 succeed 是什么就好了, 而这将通过 driver loop 传入
             (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))
```



### 处理 sequence 求值

在遇到 expression sequence 时 , 我们往往会遇到多个表达式并行的场景, 我们则需要针对这个 sequence 的每一个表达式, 逐个包装并传入外界 succeed, failed 相关的处理过程.  其中对于下一个表达式的求值被封装在上一个表达式求值的 succeed 中. 当执行到最后一个 proc 时, 函数将不会被 loop 包装, 因此会直接单独执行 proc 的求值和后续的 succeed / failed continuation.

```scheme
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail)
      (proc1 env
             ;; success continuation
             (lambda (proc1-value fail2)
               ;; success will handle proc2
               ;; 所以可以看到，程序的下一步执行本质上是由这些语法解释结构自身实现的
               ;; 而不是一个通用结构处理
               (proc2 env succeed fail2))
             ;; failure continuation, 最初由外界表达式结构执行时传入,
             fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        ;; 递归嵌套函数执行
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ([procs (map analyze exps)])
    (if (null? procs) (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))
```



### 处理赋值

对于赋值, 我们的 amb 也是可以执行 “回溯” 能力的, 也就是将 set! 的结果还原回去, 这本质上其实只是保存了 set! 之前的变量值. 在 fail 被调用的时候将原始值赋值回去而已.

```scheme
(define (analyze-assignment exp)
  (let ([var (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env succeed fail)
      (vproc
       env
       ;; success continuation
       (lambda (val fail2)
         (let ([old-value (lookup-variable-value var env)])
           (set-variable-value! var val env)
           ;; 执行后续操作
           (succeed
            'ok
            ;; 如果 succeed 所代表的后续执行过程失败，fail2-continuation 本身会被调用
            ;; 不过我们要把 fail2 包装一下，还原赋值操作
            (lambda ()
              (set-variable-value! var old-value env)
              (fail2)))))
       ;; fail continuation
       fail))))
```



### 处理过程应用 (Procedure Application)

过程的应用依旧分为 primitive procedure 和 compound procedure 两种, 对于前者我们不做任何动作. 对于后者, 我们需要类似 sequence 一样的去处理其 args 的求值, 因为 args 有可能是多个.

```scheme
(define (analyze-application exp)
  (let ([fproc (analyze (operator exp))]
        [aprocs (map analyze (operands exp))])
    (lambda (env succeed fail)
      ;; 必须 (fproc env) 执行成功了 (获取 operator)
      ;; 再去使用产出的实际 proc 去执行
      (fproc
       env
       (lambda (proc fail2)
         ;; get-args 针对整个 arguments 构成的 list 求值
         ;; 而不是像以前一样用一个 map 一个一个的求值
         (get-args
          aprocs
          env
          (lambda (args fail3)
            (execute-application proc args succeed fail3))
          fail2))
       fail))))

;; get-args 通过在 success 中递归调用 get-args 去实现
;; 在递归的 get-args 中，success 会被一层一层的用 lambda 包起来
;; 最终的 succeed 会拼接所有 args, 并交给最外面的那个 succeed 去
;;
;; 当最外层的 succeed 由于 cdr aprocs 会变成 nil
;; 触发 (succeed '())
;; 然后 => (succeed-outer (cons arg-outer '())))
;;       => (succeed-outer-outer (cons arg-outer-outer (cons arg-outer '()))))
;; ... 最终将所有的 args 拼起来给最外层的 succeed (从而 execute-application)
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

;; application 的执行没有什么变化
(define (execute-application proc args succeed fail)
  (cond
    [(primitive-procedure? proc)
     (succeed (apply-primitive-procedure proc args) fail)]
    [(compound-procedure? proc)
     ((procedure-body proc)
      (extend-environment (procedure-parameters proc)
                          args
                          (procedure-environment proc))
      succeed
      fail)]
    [else
     (error "Unknown procedure type -- EXECUTE-APPLICATION"
            proc)]))

```



### 处理驱动循环

最后, 我们需要考虑最外层的 succeed 和 failed continuation 是怎么来的, 最后的 succeed 和 fail 会做什么?

1. succeed 应该将结果打印出来, 并能够接受用户输入 “try-again" 以获取下一个可能结果.
2. fail 应该提示用户此处没有更多值能够执行了

```scheme
(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ([input (read)])
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     ;; next-alternative 本质上是内部的 fail
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       ;; next-alternative 会变成下一轮的 try-again, 在用户输入 try-again 时被触发
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       driver-loop))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;;There is not current problem")
     (driver-loop))))
```

