#lang sicp

(#%require "../utils.rkt")
(#%require "../evaln.rkt")
(#%require "../special-forms/begin.rkt")
(#%require "../special-forms/if.rkt")

;; 派生表达式 Derived Expression
;; 基于其他特殊形式的表达式定义出来的特殊形式，不用直接去实现
;; 它可能使用 eval
;; 例如 cond 就可以看成嵌套执行 if

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp)
  (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-=>-clause? clause)
  (eq? '=> (cadr clause)))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond-=>recipient clause)
  (caddr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      `false
      (let ([first (car clauses)] [rest (cdr clauses)])
        (cond
          [(cond-else-clause? first)
           ;; 基准情形，分析到了 else，且 else 理应是最后一个 clause, 如果不是就会抛错
           (if (null? rest)
               (sequence->exp (cond-actions first))
               (error "ELSE clause isn't last -- COND->IF"
                      clauses))]
          [(cond-=>-clause? first)
           (let ([predicate (cond-predicate first)]
                 [recipient (cond-=>recipient first)])
             (make-if predicate
                      ;; 将求值操作转化为 过程调用, 这就是我们要的结果，这最终也会被求值
                      (list recipient predicate)
                      (expand-clauses rest)))]
          [else
           (make-if (cond-predicate first)
                    (sequence->exp (cond-actions first))
                    (expand-clauses rest))]))))

(define (analyze-cond exp)
  (lambda (env) ((analyze (cond->if exp)) env)))

(#%provide analyze-cond)
