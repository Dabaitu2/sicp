#lang racket
(#%require "./tag-tools.rkt")
(#%require "./env.rkt")
;; (#%require "./coercion.rkt")

;; dotted tail 标记法，args 是剩余参数的 list
;; 假设 get 存在
;; apply 也是一个 primitive 基本过程, 它用于将一个参数 list 应用给一个过程
;; 其实这就已经开始贴近泛型实现了?

;; 根据 args 中的每个元素的 typed tags 集合 来 get 对应的 op 对应的基本过程
;; 这里就像是在根据函数的参数类型去寻找对应的实现一样

;; 对于跨类型的调用, 我们尝试首先去 get-coercion, 找不到再报错
;; (define (apply-generic op . args)
;;   (define (no-method type-tags)
;;     (error "No method for these types" (list op type-tags)))
;;
;;   (let ([type-tags (map type-tag args)])
;;     (let ([proc (get op type-tags)])
;;       (if proc
;;           (apply proc (map contents args))
;;           (if (= (length args 2))
;;               (let ([type1 (car type-tags)]
;;                     [type2 (cadr type-tags)]
;;                     [a1 (car args)]
;;                     [a2 (cadr args)])
;;                 (if (eq? type1 type2)
;;                     (no-method type-tags)
;;                     (let ([t1->t2
;;                            (get-coercion type1 type2)]
;;                           [t2->t1
;;                            (get-coercion type2 type1)])
;;                       (cond
;;                         [t1->t2
;;                          (apply-generic op (t1->t2 a1) a2)]
;;                         [t2->t1
;;                          (apply-generic op a1 (t2->t1 a2))]
;;                         [else (no-method type-tags)]))))
;;               (no-method type-tags))))))

;; 习题 82
;; 此处要想对超过两个参数生效需要改通用实现
;; (define (apply-generic op . args)
;;
;;   (define type-tags (map type-tag args))
;;
;;   (define (try-coercion source target-type)
;;     (if (eq? (type-tag source) target-type)
;;         source
;;         (let ([source->target
;;                (get-coercion (type-tag source)
;;                              target-type)])
;;           (if source->target
;;               (source->target source)
;;               source))))
;;
;;   (define (try-coercion-all target-type)
;;     (let ([raised-args
;;            (map (lambda (arg)
;;                   (try-coercion arg target-type))
;;                 args)])
;;       (let ([proc (get op (map type-tag raised-args))])
;;         (if proc (apply proc (map contents raised-args)) #f))))
;;
;;   (define (enumerate-types lst)
;;     (if (null? lst)
;;         (error "No coersion strategy for these types"
;;                (list op type-tags))
;;         (let ([cur-type (car lst)])
;;           (let ([try-applyed-rst
;;                  (try-coercion-all cur-type)])
;;             (if try-applyed-rst
;;                 try-applyed-rst
;;                 (enumerate-types (cdr lst)))))))
;;
;;   (let ([proc (get op type-tags)])
;;     (if proc
;;         (apply proc (map contents args))
;;         (enumerate-types type-tags))))
;;
;; (define (level type)
;;   (cond
;;     [(eq? type 'scheme-number) 0]
;;     [(eq? type 'rational) 1]
;;     [(eq? type 'complex) 2]
;;     [else (error "Invalid type: LEVEL" type)]))

;; 习题 84 + 85
(define (level type)
  (cond
    [(eq? type 'integer) 0]
    [(eq? type 'rational) 1]
    [(eq? type 'real) 2]
    [(eq? type 'complex) 3]
    [else (error "Invalid type: LEVEL" type)]))

(define (equ? x y)
  (apply-generic 'equ? x y))

(define (drop x)
  (let ([project-fn (get 'project (list (type-tag x)))])
    (if project-fn
        (let ([projected-rst (project-fn (contents x))])
          (let ([raise-fn (get 'raise
                               (list (type-tag
                                      projected-rst)))])
            (if (equ? (raise-fn projected-rst) x)
                (drop projected-rst)
                x)))
        x)))

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
    (define is_not_multiple_op 
      (> 1 (length type-tags)))
    (if proc
        (let ([res (apply proc (map contents args))])
          (if is_not_multiple_op (drop res) res))
        (let ([highest-type (get-highest-type type-tags)])
          (let ([try-raised-rst (raise-to-common
                                 highest-type)])
            (if try-raised-rst
                (if is_not_multiple_op
                    (drop try-raised-rst)
                    try-raised-rst)
                (no-method type-tags)))))))

(#%provide apply-generic drop)
