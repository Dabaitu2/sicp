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
(define (cond->if exp env)
  (expand-clauses (cond-clauses exp) env))

(define (expand-clauses clauses env)
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
             (make-if
              predicate
              ;; 此时的 recipient 还是一个没有求值完的表达式变量
              ;; 要在环境中找到对应的值, 才能去交给 apply
              (apply
               (eval recipient env)
               ;; 同样的 predicate 也只是一个表达式，没有求值，需要求出值了再送给 apply
               (list (eval predicate env)))
              (expand-clauses rest)))]
          [else
           (make-if (cond-predicate first)
                    (sequence->exp (cond-actions first))
                    (expand-clauses rest))]))))

(define (eval-cond exp env)
  (eval (cond->if exp env) env))

(#%provide eval-cond)
