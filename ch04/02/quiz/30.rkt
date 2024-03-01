#lang sicp

;; 1. 如何理解 CY 说的 (eval-sequence) 只有最后一个会被求值?
;; 因为我们 eval-sequence 里的操作是把每一个元素都 eval 一下，最后返回的是最后一个元素的 eval, 此时所有的元素都还只是 thunk
;; 而在我们的解释器中，最外层实际上是 output，它会被 actual-value 包一下，里面的 eval 返回的结果最终会被实际求值
;; 因此其他元素都只是 thunk, 只有最后一个元素被求值

;; 2. 对于 Ben 说的方案
(define (for-each proc items)
  (if (null? items)
      'done
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))

;;; L-Eval input:
(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))
;; 57
;; 321
;; 88
;;; L-Eval value:
;; done
;; 由于ben 提供的表达式中的 body 作为一个 sequence
;; 里面的每一个元素都是 primitive-procedure 因此在 apply 的时候，
;; 其中的 eval-sequence 面对的 procedure 内容全都是 primitive-procedure
;; 不会被解释为 thunk 而会被直接求值

;; 3. Cy 给出的例子
(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

;; 第一个输出会是 (1, 2) 因为 (set!) 表达式是 primitive
;; 
;; 而第二个就只会输出 1 了，因为 (set!) 被作为参数传给了 p，从而 delay 变成了 thunk 并被绑定到 env 中
;; (可以参考 apply 里面处理 compound-procedure 时对所有 operands 做了 delay-it 的操作)
;;
;; 紧接着我们执行 body, body 是一个 sequence
;; 其中第一个是 e 变量，我们 eval 变量，仅仅只是 lookup 它的值，同样不去执行这个 thunk *(没有 actual-value)
;; 而 x 作为最后一个元素被返回给用户，因此被 actual-value 处理，从而返回传入的参数也就是 1
;; 在下面，Cy 给出了他想要改造的求值方案：
;; 最大变化便是除了最后一个使用 eval（因为最终的返回会在 output 前被 actual-value 给 force 求值 , 其他的都使用 actual-value 而非 eval 执行
(define (eval-sequence exps env)
  (cond
    [(last-exp? exps) (eval (first-exp exps) env)]
    [else
     (actual-value (first-exp exps) env)
     (eval-sequence (rest-exps exps) env)]))

;; 根据他修改的例子，我们的输出会是
;; (p1 1) => (1, 2) 保持不变
;; (p2 1) => (1, 2) body 是一个 list  (e x) , e 会被使用 actual-value 求值，从而导致参数传入的 (set!) 被实际求值
;; 从而导致 set！被调用，x 的值被改变为 (cons x '(2)), 最后的结果成为 (list 1 2)


;; 4. 由于 actual-value 对于非 thunk 的结果，依然可以正常的返回，因此即使使用 Cy 的方法
;; Ben 给出的 case 也可以正常使用


;; 5. 其实个人比较喜欢 Cy 的办法, 因为目前的这个解释器实际上已经引入了副作用 (例如 set!)
;; 那么在这个情况下，我们就不是在追寻纯粹的数学函数方案了, 在这个细节上去追求无副作用个人感觉意义不大 
;; 如果想要让一切副作用都不发生，那么甚至不应该引入 set!
