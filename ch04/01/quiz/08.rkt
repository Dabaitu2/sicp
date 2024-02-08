#lang sicp

;; achieve named-let
;; (let <var> <bindings> <body>)
;; we give let a name so we can repeatly invoke the internal procedure by calling its name
(define (fib n)
  (let fib-iter ([a 1] [b 0] [count n])
    (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))

(#%require "../evaluator/utils.rkt")
(#%require "../evaluator/special-forms.rkt")

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

(define (make-let bindings body)
  ;; body 可能是一个 list, 所以用 cons 去连接
  (cons 'let (cons bindings body)))

(define (let->combination exp)
  ;; 处理 命名 let
  (if (let-named-clause? exp)
      (let ([name (let-named-name exp)]
            [clause (let-named-clause exp)])
        (let ([bindings (let-bindings clause)]
              [body (let-body clause)])
          (let ([vars (let-vars bindings)]
                [vals (let-values bindings
                        )])
            (let ([proc (make-lambda vars body)])
              (cons (make-lambda
                     '()
                     (list (cons 'define (list name proc))
                           (cons name vals)))
                    '())))))
      (let ([bindings (let-bindings exp)])
        (cons (make-lambda (let-vars bindings)
                           (let-body exp))
              (let-values bindings
                )))))

(define (eval-let exp env)
  (eval (let->combination (let-clauses exp)) env))
