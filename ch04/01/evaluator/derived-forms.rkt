#lang sicp

(#%require "./utils.rkt")
(#%require "./sequence.rkt")
(#%require "./evaln.rkt")
(#%require "./special-forms.rkt")

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

(define (and? exp)
  (tagged-list? exp 'and))
(define (and-clauses exp)
  (cdr exp))
(define (expand-and exps)
  (if (no-exps exps)
      'true
      (make-if (first-exp exps)
               (expand-and (rest-exps exps))
               'false)))
(define (and->if exp)
  (expand-and (and-clauses exp)))
(define (eval-and exp env)
  (eval (and->if exp) env))

(define (or? exp)
  (tagged-list? exp 'or))
(define (or-clauses exp)
  (cdr exp))
(define (expand-or exps)
  (if (no-exps exps)
      'false
      (make-if (first-exp exps)
               'true
               (expand-or (rest-exps exps)))))
(define (or->if exp)
  (expand-or (or-clauses exp)))
(define (eval-or exp env)
  (eval (or->if exp) env))

(define (let? exp)
  (tagged-list? exp 'let))
(define (let-clauses exp)
  (cdr exp))
(define (let-bindings clause)
  (car clause))
(define (let-named-clause? clause)
  (symbol? (car clause)))
(define (let-named-name clause)
  (car clause))
(define (let-named-clause clause)
  (car clause))
(define (let-vars bindings)
  (if (null? bindings)
      '()
      (cons (caar bindings) (let-vars (cdr bindings)))))
(define (let-values bindings
          )
  (if (null? bindings)
      '()
      ;; 所有的表达式默认都是 list 而非 cons 组成
      (cons (cadar bindings)
            (let-values (cdr bindings)
              ))))

(define (let-body clause)
  (cdr clause))

;; let->组合式
;; combination 就是 (list procedure parameters)
(define (let->combination exp)
  ;; 处理 命名 let
  (if (let-named-clause? exp)
      (let ([var (let-named-name exp)]
            [defs (let-named-clause exp)])
        (eval (make-let (list var defs) (let-body defs))))
      (let ([bindings (let-bindings exp)])
        (list (make-lambda (let-vars bindings)
                           (let-body exp))
              (let-values bindings
                )))))

(define (make-let bindings body)
  ;; body 可能是一个 list, 所以用 cons 去连接
  (cons 'let (cons bindings body)))
(define (eval-let exp env)
  (eval (let->combination (let-clauses exp)) env))

;; let*
(define (let*? exp)
  (tagged-list? exp 'let*))
(define (let*-bindings clause)
  (car clause))
(define (let*-body clause)
  (cdr clause))

;; 将 let* 转换为多个 let 嵌套
(define (let*->nested-lets exp)
  (define (expand-clauses bindings body)
    (if (null? bindings)
        body
        (let ([first (car bindings)] [rest (cdr bindings)])
          (make-let (list first)
                    (expand-clauses rest body)))))
  (expand-clauses (let*-bindings exp) (let*-body exp)))

(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

(define (while? exp)
  (tagged-list? exp 'while))
(define (make-while predicate body)
  (cons 'while (cons predicate body)))
(define (while-clauses exp)
  (cdr exp))
(define (while-predicate clause)
  (car clause))
(define (while-body clause)
  (cdr clause))
(define (while->combination exp)
  (let ([predicate (while-predicate exp)]
        [body (while-body exp)])
    (make-if
     predicate
     ;; 这里的 body 是一个 list，而 if 接受的实际上是单个表达式，所以需要用 begin 包装一下
     ;; 这里不用 apply，是因为 if 的 alternative / consequence 求值是运行时，不是编译时，所以这里就是一个表达式
     (sequence->exp (list body exp))
     'done)))
(define (eval-while exp env)
  (eval (while->combination exp) env))

(define (install-derived-form-package)
  (put eval-cond 'exp 'cond)
  (put eval-and 'exp 'and)
  (put eval-or 'exp 'or)
  (put eval-let 'exp 'let)
  (put eval-let* 'exp 'let*)
  (put eval-while 'exp 'while))

(#%provide install-derived-form-package)
