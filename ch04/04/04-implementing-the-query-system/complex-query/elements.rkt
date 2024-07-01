#lang sicp

;; predicates
(define (empty-conjunction? exps)
  (null? exps))
;; selectors
(define (first-conjunct exps)
  (car exps))
(define (rest-conjuncts exps)
  (cdr exps))

(define (empty-disjunction? exps)
  (null? exps))
(define (first-disjunct exps)
  (car exps))
(define (rest-disjuncts exps)
  (cdr exps))

;; selectors for negate operations
;; 不是对 query 取反，只是获取这个 query，
;; 检查是否有 frame 针对这个 query 查询的结果为空, 如有 那个 frame 对应的变量的 data 则说明满足条件
(define (negated-query exps)
  (car exps))

;; selectors for lisp-value operaions
;; 获取表达式的实际部分
;;             predicate     args
;; (lisp-value >             s2 s1)
(define (predicate exps)
  (car exps))
(define (args exps)
  (cdr exps))

(#%provide empty-conjunction?
           first-conjunct
           rest-conjuncts
           empty-disjunction?
           first-disjunct
           rest-disjuncts
           negated-query
           predicate
           args)
