#lang sicp
(#%require "../qeval.rkt")
(#%require "./elements.rkt")
(#%require "../registry.rkt")
(#%require "../core/instantiate.rkt")
(#%require "../../../../common/data/stream.rkt")
(#%require "../core/binding.rkt")
(#%require "../core/unify-match.rkt")

;; parallel conjoin operation
;; however, here's a issue:
;; as chapter 4.4.3 said, if the operands `not` accept an empty-stream without any frame in it
;; it might return false result, cause the empty stream input to `not` will directly return empty
;; and furtherly make `not` return empty-stream, which is not correct
;; TODO:  we will try to fix this after quiz 4.77
(define (new-conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (unify-stream (qeval (first-disjunct conjuncts)
                           frame-stream)
                    (new-conjoin (rest-conjuncts conjuncts)
                                 frame-stream))))

(define (unify-stream s1 s2)
  (cond
    [(stream-null? s1) s2]
    [(stream-null? s2) s1]
    [else
     (stream-flatmap
      (lambda (frame1)
        (stream-flatmap (lambda (frame2)
                          (unify-frame frame1 frame2))
                        s2))
      s1)]))

;; unify 两个 frame ，任何一个 binding 不满足条件就直接返回 the-empty-stream
;; 否则走到递归的终点 (frame1 遍历完毕), 返回被 extend 完成的 frame2
(define (unify-frame frame1 frame2)
  ;; 如果 frame2 直接不存在， 那说明子查询都没有符合条件的， and 下去肯定就更没有结果了
  (if (null? frame2)
      the-empty-stream
      ;; 递归遍历 frame1 (cdr), 因此一定是 frame1 会没有
      (if (null? frame1)
          (singleton-stream frame2)
          (let ([binding (car frame1)])
            (let ([var (binding-variable binding)]
                  [val (binding-value binding)])
              (let ([unify-result
                     (unify-match var val frame2)])
                (if (eq? unify-result 'failed)
                    the-empty-stream
                    (unify-frame (cdr frame1)
                                 unify-result))))))))

;; 利用 conjoin(合取) 处理 Add 查询, 本质上就是递归对每一个内部的查询处理, 有点像 fold
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

;; 利用 disjoin(析取) 处理 Or 查询, 对每一个内部的查询产生的结果进行 merge,
;; interleave 是为了保证 delay 的数据可以也有机会被读取到
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

;; 利用 negate(取反/否定) 处理 Not 查询
;; negate 的本质是过滤器, 也就是说它只能基于已有的 frame 去工作
;; 它 基于现有的 frame 去检查是否满足内部的查询, 如果满足就丢弃这个 frame
;; 因此如果 直接单独调用 negate 
;; 1. 由于 frame 是空的，那么肯定很有可能返回非空的结果(只要查询是合法的), 从而使得 negate 取反后什么都不输出，和预计不符
;; 2. 如果用户输入了不存在的查询格式， 那么 qeval 肯定返回空， 反倒使得 negate 会产生输出， 这依然是和逻辑不一致的
;; 想要正确的使用 not, 必须和 and 共用
(define (negate operands frame-stream)
  (simple-stream-flatmap
   ;; 对于每一个 frame
   (lambda (frame)
     ;; 如果使用传入的查询 执行该 frame 的结果为空，说明这个 frame 符合条件，返回之
     (if (stream-null?
          (qeval (negated-query operands)
                 ;; 通过 单个 frame 创建一个只有一个元素 (frame) 的 stream
                 (singleton-stream frame)))
         (singleton-stream frame)
         ;; 否则就返回空 stream
         the-empty-stream))
   frame-stream))

;; 利用 lisp-value处理 lisp表达式 过滤
;; 此 lisp-value 不是用户直接使用的那个哦
(define (lisp-value call frame-stream)
  (simple-stream-flatmap
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

(define (uniquely-asserted operands frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (let ([result-frame (qeval (unique-query operands)
                                (singleton-stream frame))])
       (if (stream-single? result-frame)
           result-frame
           the-empty-stream)))
   frame-stream))

(define (init-compound-query)
  (put conjoin 'qeval 'and)
  ;; (put new-conjoin 'qeval 'and)
  (put disjoin 'qeval 'or)
  (put negate 'qeval 'not)
  (put lisp-value 'qeval 'lisp-value)
  (put always-true 'qeval 'always-true)
  (put uniquely-asserted 'qeval 'unique))

(#%provide init-compound-query)
