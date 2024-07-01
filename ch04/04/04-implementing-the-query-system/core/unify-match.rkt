#lang sicp
(#%require "../predicates.rkt")
(#%require "../core/binding.rkt")

;; 核心: Unification
;; 本质上它是一种更通用的 pattern-match, 因为它分析的是
;; 两个 pattern 是否可以被认为能表示同样的东西
(define (unify-match p1 p2 frame)
  (cond
    [(eq? frame 'failed) 'failed]
    [(equal? p1 p2) frame]
    ;; 由于 p1, p2 两者都是可能变量，所以 cond 要涉及两者
    ;; 不过 cond 是只要命中一个条件就会结束，这个跟 switch 的 case 还是不一样的
    [(var? p1) (extend-if-possible p1 p2 frame)]
    [(var? p2) (extend-if-possible p2 p1 frame)]
    ;; 如果两者都是 pair 说明还是一个复合的 pattern 结构
    ;; 我们需要递归的拆分直到两者都为基本的 var / val
    [(and (pair? p1) (pair? p2))
     (unify-match (cdr p1)
                  (cdr p2)
                  (unify-match (car p1) (car p2) frame))]
    [else 'failed]))

(define (extend-if-possible var val frame)
  (let ([binding (binding-in-frame var frame)])
    (cond
      [binding
       (unify-match (binding-value binding) val frame)]
      ;; 相较于 extend-if-consistent 多了两个检查
      ;; 1. 如果 val 本身也是个变量，则尝试先在 frame 中寻找 val 对应的 binding
      ;;    然后重新进行一次 unify-match 和 extend 工作
      [(var? val)
       (let ([binding (binding-in-frame val frame)])
         (if binding
             (unify-match var (binding-value binding) frame)
             (extend var val frame)))]
      ;; 2. 如果 val 不是变量，但是是一个依赖于 var 的表达式
      ;; 我们也要拒绝这种情形。
      ;; (例如 unify (hello ?x ?y) (hello ?x (hi ?z ?y))
      ;; 如果将 ?y 定义为 (hi ?z ?y) 我们需要想办法找到是否有一个 ?y 的具体值，使得 ?y = （hi ?z ?y)
      ;; 本质上，这就是求解一个方程  ?y = 关于?y 的表达式 的一个不动点 (一个解)
      ;; 而没有一个一般性的方法能实现这个(在数学上亦需要具体问题具体分析)，因此直接拒绝掉
      [(depends-on? val var frame) 'failed]
      [else (extend var val frame)])))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond
      [(var? e)
       (if (equal? var e)
           true
           (let ([b (binding-in-frame e frame)])
             (if b (tree-walk (binding-value b)) false)))]
      [(pair? e)
       (or (tree-walk (car e)) (tree-walk (cdr e)))]
      [else false]))
  (tree-walk exp))

(#%provide unify-match)
