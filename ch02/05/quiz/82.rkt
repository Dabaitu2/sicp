#lang sicp

;; 这个给出的 coercion 策略的不够通用的问题在于, 如果存在一个方法，其所接受的类型高于目前传入参数的所有
;; 类型 (Supertype) 则会导致所有参数无法 raise 到那个 supertype, 因为仅仅尝试了参数所提供的那部分类型
;; 比如对于类型 A<B<C , 我们存在函数 hello (A C B) , 但传入参数 (A B B) 的话，就没办法向上 raise 到 C 了

(#%require
 "../02-combination-data-of-different-types/tag-tools.rkt")
(#%require
 "../02-combination-data-of-different-types/env.rkt")
(#%require
 "../02-combination-data-of-different-types/coercion.rkt")

(define (apply-generic op . args)

  (define type-tags (map type-tag args))

  (define (no-method type-tags)
    (error "No method for these types" (list op type-tags)))

  (define (try-coercion source target-type)
    (if (eq? (type-tag source) target-type)
        source
        (let ([source->target
               (get-coercion (type-tag source)
                             target-type)])
          (if source->target
              (source->target source)
              source))))

  (define (try-coercion-all target-type)
    (let ([raised-args
           (map (lambda (arg)
                  (try-coercion arg target-type))
                args)])
      (let ([proc (get op (map type-tag raised-args))])
        (if proc
            (apply proc (map contents raised-args))
            #f))))

  (define (enumerate-types lst)
    (if (null? lst)
        (error "No coersion strategy for these types"
               (list op type-tags))
        (let ([cur-type (car lst)])
          (let ([try-applyed-rst
                 (try-coercion-all cur-type)])
            (if try-applyed-rst
                try-applyed-rst
                (enumerate-types (cdr lst)))))))

  (let ([proc (get op type-tags)])
    (if proc
        (apply proc (map contents args))
        (enumerate-types type-tags))))

(#%provide apply-generic)
