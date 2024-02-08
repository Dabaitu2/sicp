#lang sicp

(#%require "../utils.rkt")
(#%require
 "../../../../common/data/conventional-interface.rkt")

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
  (cdr clause))
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

(define (make-let bindings body)
  ;; body 是一个 list, 所以用 cons 去连接
  (cons 'let (cons bindings body)))

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
                    (list (expand-clauses rest body))))))
  (expand-clauses (let*-bindings exp) (let*-body exp))
  )

;; 支持变量提升和嵌套递归定义的 let
(define (letrec? exp)
  (tagged-list? exp 'letrec))
(define (letrec-clause exp)
  (cdr exp))
(define (letrec-bindings clause)
  (car clause))
(define (letrec-body clause)
  (cdr clause))

(define (letrec->let exp)
  (let ([clause (letrec-clause exp)])
    (let ([bindings (letrec-bindings clause)]
          [body (letrec-body clause)])
      (let ([result (fold-right
                     (lambda (cur acc)
                       (let ([let-var (list (car cur)
                                            ;; let 的 binding 自身会被 eval 解析一次
                                            ;; 所以要多加一个引号
                                            ''*unassigned*)]
                             [let-set (list 'set!
                                            (car cur)
                                            (cadr cur))])

                         (list (cons let-var (car acc))
                               (cons let-set (cadr acc)))))
                     (list '() body)
                     bindings)])
        (let ([new-bindings (car result)]
              [new-body (cadr result)])
          (make-let new-bindings new-body))))))

(#%provide make-let
           let-named-name
           let-named-clause
           let-named-clause?
           let-vars
           let-body
           let-bindings
           let-values
           let*->nested-lets
           let-clauses
           letrec?
           letrec->let)
