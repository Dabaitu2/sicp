#lang sicp

(#%require "../utils.rkt")

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
(define (let-values bindings)
  (if (null? bindings)
      '()
      ;; 所有的表达式默认都是 list 而非 cons 组成
      (cons (cadar bindings)
            (let-values (cdr bindings)
              ))))

(define (let-body clause)
  (cdr clause))

(define (make-let bindings body)
  ;; body 可能是一个 list, 所以用 cons 去连接
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
                    (expand-clauses rest body)))))
  (expand-clauses (let*-bindings exp) (let*-body exp)))

(#%provide make-let
           let-named-name
           let-named-clause
           let-named-clause?
           let-vars
           let-body
           let-bindings
           let-values
           let*->nested-lets
           let-clauses)
