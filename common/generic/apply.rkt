#lang racket

(#%require "./tag.rkt")

;; 需要的参数都是透传的 带 tag 参数
;; 1. 通过所有参数的 tag 和 op 获取到对应的具体类型实现
;; 2. 调用实现，传入所有参数的 content 作为实参数
(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(#%provide apply-generic)
