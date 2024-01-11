#lang sicp

;; achieve let* expression
;; the binding will be evaluate in order,
;; so we can get previous defination right after last binding-creating

(let* ([x 3] [y (+ x 2)] [z (+ x y 5)]) (* x z)) ;; -> 39

;; 一个 let* 可以写成 let 的多个嵌套形式
(let ([x 3])
  (let ([y (+ x 2)]) (let ([z (+ x y 5)]) (* x z))))

(#%require "../evaluator/utils.rkt")
(#%require "../evaluator/special-forms.rkt")

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (let*? exp)
  (tagged-list? exp 'let*))
(define (let*-bindings clause)
  (car clause))
(define (let*-body clause)
  (cdr clause))

(define (expand-clauses bindings body)
  (if (null? bindings)
      body
      (let ([first (car bindings)] [rest (cdr bindings)])
        (make-let (list first)
                  (expand-clauses rest body)))))

;; 将 let* 转换为多个 let 嵌套
(define (let*->nested-lets exp)
  (expand-clauses (let*-bindings exp) (let*-body exp)))

(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

;; I didn't see any neccessity to explicit achieve let*
;; we should just use above implementation to do so
