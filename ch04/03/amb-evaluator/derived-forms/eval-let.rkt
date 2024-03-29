#lang sicp

(#%require "../evaln.rkt")
(#%require "../special-forms/lambda.rkt")
(#%require "./let.rkt")
(#%require
 "../../../../common/data/conventional-interface.rkt")

;; let->组合式
;; combination 就是 (list procedure parameters)
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
              ;; 由于要立即执行一次这个 let
              ;; 并且 let 中可能立即需要 name 对应的变量
              ;; 所以我们要手动构造一个过程调用
              ;; 去把 define 操作当场完成
              ;; 因此这里不使用 sequnce->exp 去构造 begin
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

;; 将 let* 转换为多个 let 嵌套
(define (let*->nested-lets exp)
  (define (expand-clauses bindings body)
    (if (null? bindings)
        body
        (let ([first (car bindings)] [rest (cdr bindings)])
          (make-let (list first)
                    (list (expand-clauses rest body))))))
  (expand-clauses (let*-bindings exp) (let*-body exp)))

(define (letrec->let exp)
  (let ([clause (letrec-clause exp)])
    (let ([bindings (letrec-bindings clause)]
          [body (letrec-body clause)])
      (let ([result
             (fold-right
              (lambda (cur acc)
                (let ([let-var
                       (list (car cur)
                             ;; let 的 binding 自身会被 eval 解析一次
                             ;; 所以要多加一个引号
                             ''*unassigned*)]
                      [let-set
                       (list 'set! (car cur) (cadr cur))])

                  (list (cons let-var (car acc))
                        (cons let-set (cadr acc)))))
              (list '() body)
              bindings)])
        (let ([new-bindings (car result)]
              [new-body (cadr result)])
          (make-let new-bindings new-body))))))

(define (analyze-let exp)
  (analyze (let->combination (let-clauses exp))))

(define (analyze-let* exp)
  (analyze (let*->nested-lets (let-clauses exp))))

(define (analyze-letrec exp)
  (analyze (letrec->let exp)))

(#%provide analyze-let analyze-let* analyze-letrec)
