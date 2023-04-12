#lang sicp
(#%require "./tag-tools.rkt")
(#%require "./env.rkt")

;; dotted tail 标记法，args 是剩余参数的 list
;; 假设 get 存在
;; apply 也是一个 primitive 基本过程, 它用于将一个参数 list 应用给一个过程
;; 其实这就已经开始贴近泛型实现了?

;; 根据 args 中的每个元素的 typed tags 集合 来 get 对应的 op 对应的基本过程
;; 这里就像是在根据函数的参数类型去寻找对应的实现一样
(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))


(#%provide apply-generic)
