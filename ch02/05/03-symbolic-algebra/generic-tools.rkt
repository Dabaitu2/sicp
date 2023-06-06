#lang racket
(#%require "./tag-tools.rkt")
(#%require "./env.rkt")
;; (#%require "./coercion.rkt")

(define (level type)
  (cond
    [(eq? type 'integer) 0]
    [(eq? type 'rational) 1]
    [(eq? type 'real) 2]
    [(eq? type 'complex) 3]
    [(eq? type 'term) 4]
    [(eq? type 'sparse) 5]
    [(eq? type 'dense) 6]
    [(eq? type 'polynomial) 7]
    [else (error "Invalid type: LEVEL" type)]))

(define (equ? x y)
  (apply-generic 'equ? x y))

(define (drop x)
  (if (is-datum? x)
      (let ([project-fn (get 'project (list (type-tag x)))])
        (if project-fn
            (let ([projected-rst (project-fn (contents x))])
              (let ([raise-fn (get 'raise
                                   (list (type-tag
                                          projected-rst)))])
                (if (equ? (raise-fn projected-rst) x)
                    (drop projected-rst)
                    x)))
            x))
      x
      )
  )

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
  ;; 逐层向上 raise 而不是一口气 raise 上去，所以只要能够间接 raise 就可以了
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
            (no-method type-tags)))))

  (let ([proc (get op type-tags)])
    (define is_not_multiple_op (> 2 (length type-tags)))
    (if proc
        (let ([res (apply proc (map contents args))])
          (drop res)
          #| (if is_not_multiple_op (drop res) res) |#
          )
        (let ([highest-type (get-highest-type type-tags)])
          (let ([try-raised-rst (raise-to-common
                                 highest-type)])
            (drop try-raised-rst)
            #| (if is_not_multiple_op |#
            #|     (drop try-raised-rst) |#
            #|     try-raised-rst) |#
            )))))

(#%provide apply-generic drop)
