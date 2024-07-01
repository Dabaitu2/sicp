#lang sicp
(#%require "../qeval.rkt")
(#%require "./elements.rkt")
(#%require "../registry.rkt")
(#%require "../core/instantiate.rkt")
(#%require "../../../../common/data/stream.rkt")

;; 利用 conjoin(合取) 处理 Add 查询, 本质上就是递归对每一个内部的查询处理, 有点像 fold
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

;; 利用 disjoin(析取) 处理 Or 查询, 对每一个内部的查询产生的结果进行 merge, interleave 是为了保证 delay 的数据可以也有机会被读取到
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

;; 利用 negate(取反/否定) 处理 Not 查询
(define (negate operands frame-stream)
  (stream-flatmap
   ;; 对于每一个 frame
   (lambda (frame)
     ;; 如果使用传入的查询 执行该 frame 的结果为空，说明这个 frame 符合条件，返回之
     (if (stream-null?
          (qeval (negated-query operands)
                 ;; 通过 单个 frame 创建一个只有一个元素 (frame) 的 stream
                 (singleton-stream frame)))
         (singleton-stream frame)
         ;; 否则就返回空 stream
         the-empty-stream)
     )
   frame-stream
   ))

;; 利用 lisp-value处理 lisp表达式 过滤
;; 此 lisp-value 不是用户直接使用的那个哦
(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     ;; 将 frame 中已有的变量绑定到 lisp-value 提供的 call 表达式中替换其变量(本质上还是 pattern match)
     ;; 如果结果为 true 则返回包含这个 frame 的 stream, 反之返回空 stream
     (if (execute (instantiate call frame
                    [lambda
                        (v f)
                      (error "Unknown pat var: LISP-VALUE"
                             v)]))
         (singleton-stream frame)
         the-empty-stream)
     frame-stream)))

;; 执行传给 lisp-value 的表达式, 此时表达式中的变量应该已经被替换完成
(define (execute exp)
  ;; 我们的查询语言基于一个已经支持 eval 的 lisp 实现, 其实我们之前写的 lisp 理论上也是支持的，只是我们的 lisp 没有读取文件和解析模块的能力
  ;; 这里只能使用真实 lisp 的 eval / apply 了
  ;; sicp 没有 user-initial-environment, 我们使用 scheme-report-environment 代替
  ;; lisp value 本身格式是 (lisp-value predicate . args)
  ;; 最后的执行本质上就是把 args 传给 predicate
  (apply (eval (predicate exp)
               (scheme-report-environment 5))
         (args exp)))

;; 忽略传入内容, 直接认为查询被满足
;; 例如对于 (rule (same ?x ?x)) 这种规则，我们如果走一般的处理 rule 的方式会无法处理，此时需要直接通过
;; always-true 返回传入的原始 frame-stream
(define (always-true ignore frame-stream)
  frame-stream)

(define (init-compound-query)
  (put conjoin 'qeval 'and)
  (put disjoin 'qeval 'or)
  (put negate 'qeval 'not)
  (put lisp-value 'qeval 'lisp-value)
  (put always-true 'qeval 'always-true))

(#%provide init-compound-query)
