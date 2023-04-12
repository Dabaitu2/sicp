#lang sicp
(#%require
 "../02-combination-data-of-different-types/tag-tools.rkt")
(#%require
 "../02-combination-data-of-different-types/env.rkt")

;; 实现另一种 apply-generic: 并不是尝试 coercion 所有类型到某一个参数的类型
;; 而是将所有的参数尝试按照自身 raise 的方向同时提升到某个类型
;; 这样甚至连 coercion 这一层也被我们屏蔽了，取而代之是 raise

(define (level type)
  (cond
    [(eq? type 'scheme-number) 0]
    [(eq? type 'rational) 1]
    [(eq? type 'complex) 2]
    [else (error "Invalid type: LEVEL" type)]))

;; 习题 84
(define (apply-generic op . args)

  (define type-tags (map type-tag args))

  (define (no-method type-tags)
    (error "No method for these types" (list op type-tags)))

  ;; 获取目标类型，为参数中的最高类型
  (define (get-highest-type type-tags)
    (if (null? (cdr type-tags))
        (car type-tags)
        (let ([t1 (car type-tags)]
              [t2 (get-highest-type (cdr type-tags))])
          (let ([l1 (level t1)] [l2 (level t2)])
            (if (> l1 l2) t1 t2)))))

  ;; 尝试 raise 一个元素到目标类型
  (define (raise-to-type source target-type)
    (if (eq? (type-tag source) target-type)
        source
        (let ([raise-fn
               (get 'raise (list (type-tag source)))])
          (if raise-fn
              (raise-to-type (raise-fn source) target-type)
              #f))))

  ;; 将所有元素提升到目标类型，并获取对应的方法，再将参数传入执行
  (define (raise-to-common type)
    (let ([raised-args
           (map (lambda (arg) (raise-to-type arg type))
                args)])
      (let ([proc (get op (map type-tag raised-args))])
        (if proc
            (apply proc (map contents raised-args))
            #f))))

  (let ([proc (get op type-tags)])
    (if proc
        (apply proc (map contents args))
        (let ([highest-type (get-highest-type type-tags)])
          (let ([try-raised-rst (raise-to-common
                                 highest-type)])
            (if try-raised-rst
                try-raised-rst
                (no-method type-tags)))))))
(#%provide apply-generic)
