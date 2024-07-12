#lang sicp
(#%require "../../../../common/data/stream.rkt")
(#%require "./registry.rkt")
(#%require "./predicates.rkt")
(#%require "./core/database.rkt")
(#%require "./core/elements.rkt")
(#%require "./core/pattern-match.rkt")
(#%require "./core/unify-match.rkt")
(#%require "./core/binding.rkt")

;; 我们书写这些查询的方式依然采用了 wishful thinking, 也就是从 qeval 出发，分别处理简单和符合查询的顶层设计
;; 实际的实现留到后续完成

(define (qeval query frame-stream)
  ;; 'qeval 只是 table 中的一个 group 名的感觉，在 evaln 中就是 'exp 对应的玩意
  ;; type 用于获取 query 对应的 special form, 如果找不到就认为是简单查询
  (let ([qproc (get 'qeval (type query))])
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

;; 处理简单查询，接受查询 pattern 和上层 frame-stream
;; 返回扩展后的 frame-stream
(define (simple-query query-pattern frame-stream)
  ;; 最后产生的所有结果流会被 flatmap
  (stream-flatmap
   (lambda (frame)
     ;; 简单查询可以处理两种场景：纯粹的搜索 (pattern match) 和对规则的应用 (Unification)
     ;; 因此可以看到对于任何一个 frame，我们都会将其传给两个分支: 搜索断言和应用规则
     ;; 最后将结果形成的 stream 进行合并
     ;; 通过使用 delay，我们可以使得搜索的过程被后置，这可能是一种性能优化手段?
     (stream-append-delayed
      ;; 通过 query-pattern 和 frame 去搜索 Database 是否已有 assertion 满足条件
      (find-assertions query-pattern frame)
      ;; 这里的 delay 用的是 sicp 预先定义好的，如果完全是我们自己写，我们需要基于我们之前实现的 lazy-evaluator 去解析
      ;; 对 rules 的搜索会被后置
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;; ===============Handle Assertions========================
;; 基于某个 frame 去匹配已有的 assertion
;; 最基本的查询逻辑
(define (find-assertions pattern frame)
  (simple-stream-flatmap
   (lambda (datum) (check-an-assertion datum pattern frame))
   ;; fetch-assertions 获得所有断言的 stream
   ;; 这里其实可以不使用此方法，而是直接搜索数据库, 不过 fetch-assertions 可以做一些
   ;; 预处理工作，提前去除一些不重要的数据. 一定程度上提升性能。
   (fetch-assertions pattern frame)))

;; check-an-assertion 检查断言是否可以匹配上 pattern 和 frame
(define (check-an-assertion assertion query-pat query-frame)
  (let (;; 通过模式匹配完成实际的结果, 结果可能是 'failed 也可能是一个扩充后的 frame
        [match-result
         (pattern-match query-pat assertion query-frame)])
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream
         ;; 记得继承之前的 promise
         (make-frame (frame-bindings match-result)
                     (frame-promises query-frame))))))

;; ================Handle Rules========================
;; 将目标 pattern 和 frame 作为上下文
;; (frame 理论上有可能已经匹配了部分变量)
;; 检查是否存在 rules 满足条件
(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

;; return a stream of colllections of possible frames
(define (apply-a-rule rule query-pattern query-frame)
  ;; rename the variable inside the rules to avoid conflict of names
  ;; like two rules with the same variable but refer to different target
  (let ([clean-rule (rename-variables-in rule)])
    ;; 通过 unify-match 去尝试匹配查询模式与规则的结论
    ;; unify-match 和 pattern-match 的区别在于它还会考虑变量
    ;; 理论上，unify-match 会返回一个 frame 或者 'failed
    ;; 那个 frame 会将 conclusion 中的 变量替换为 query-frame 中可能的值 (如果有的话
    (let ([unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)])
      (if (eq? unify-result 'failed)
          the-empty-stream
          ;; 如果 unify-match 成功了，那么就将规则的 body 和 unify-result 作为上下文
          ;; 进一步的执行对 body 的实际查询
          (qeval (rule-body clean-rule)
                 (singleton-stream
                  (make-frame (frame-bindings unify-result)
                              (frame-promises query-frame))
                  ))))))

(define (rename-variables-in rule)
  (let ([rule-application-id (new-rule-application-id)])
    (define (tree-walk exp)
      (cond
        [(var? exp)
         (make-new-variable exp rule-application-id)]
        [(pair? exp)
         (cons (tree-walk (car exp)) (tree-walk (cdr exp)))]
        [else exp]))
    (tree-walk rule)))

(#%provide qeval)
