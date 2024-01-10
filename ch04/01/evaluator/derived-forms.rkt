#lang sicp

(#%require "./utils.rkt")
(#%require "./special-forms.rkt")

;; 派生表达式 Derived Expression
;; 基于其他特殊形式的表达式定义出来的特殊形式，不用直接去实现
;; 例如 cond 就可以看成嵌套执行 if

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp)
  (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      `false
      (let ([first (car clauses)] [rest (cdr clauses)])
        (if (cond-else-clause? first)
            ;; 基准情形，分析到了 else，且 else 理应是最后一个 clause, 如果不是就会抛错
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            ;; 递归创建
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(#%provide cond->if)
