#lang sicp
(#%require "./binding.rkt")
(#%require "../predicates.rkt")

;; 核心: 模式匹配
;; 利用 模式 去检查 数据
;; 本质上，模式和数据都是 list 构成的
;; 因此我们可以获取到 pat 和 dat 的每一个元素，然后进行匹配
;; 如果最后的结果不是 fail，就会返回一个 frame, 这个 frame 可能是一个扩充后的 frame，扩充的 data 来自于 dat
(define (pattern-match pat dat frame)
  (cond
    [(eq? frame 'failed) 'failed]
    ;; 如果 pat he dat 一致, 说明非变量部分匹配成功
    [(equal? pat dat) frame]
    ;; 如果 pat 部分是 var，则尝试扩充 frame
    ;; 1. 如果 frame 中对于这个 pat 已经有对应的值 且和 dat 一致则返回 frame (不需要扩充)
    ;; 2. 如果 frame 中对于这个 pat 已经有对应的值，但 是这个值和当前的 dat 不匹配，则报错并传播出去
    ;; 3. 如果 frame 中没有这个 pat 的值，则扩充 frame 并返回
    [(var? pat) (extend-if-consistent pat dat frame)]
    [(and (pair? pat) (pair? dat))
     (pattern-match
      (cdr pat)
      (cdr dat)
      (pattern-match (car pat) (car dat) frame))]
    [else 'failed]))

(define (extend-if-consistent var dat frame)
  ;; 根据 pat 中的 var 去寻找 frame 中对应的值
  (let ([binding (binding-in-frame var frame)])
    (if binding
        ;; 如果存在(说明当前检查的 pattern 已经在之前为当前 pattern 中的 var 赋值过了)，利用 pattern match 重新检查
        ;; 因为 frame 中的 binding 对应的有可能是具体值也可能还是个变量，这里要递归的检查到是具体的值为止
        (pattern-match (binding-value binding) dat frame)
        ;; 如果不存在这个 binding, 说明就这个 var 还没有被赋值过，我们就可以将 dat 作为一种赋值尝试
        ;; 简单扩充这个 frame, 将当前变量绑定到这个 dat 上, 进行后续的检查
        (extend-binding var dat frame))))

(#%provide pattern-match)
