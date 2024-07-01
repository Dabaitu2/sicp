#lang sicp
(#%require "./utils.rkt")

;; 获取用户执行的查询的类型
;; 理论上所有的 query 都应该是一种 pair， 其他类型都是错误的
;; 例如
;; simple-query / rule:
;; (job ?x (computer programmer))
;; (?a next-to ?b)
;;
;; compound-query:
;; (and (job ?person (computer programmer))
;;      (address (?person where)))
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

;; 获取查询的内容, 只针对 compound-query 有效
;; 因为 simple-query 查询中的每一个东西都是 模式的一部分
;; 而 compound-query 的第一位理论上算是一种控制元素
(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression contents")))

;; 判断用户传入的是不是一个待插入的断言
(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))
(define (add-assertion-body exp)
  (car (contents exp)))

(define (var? exp)
  (tagged-list? exp '?))
(define (constant-symbol? exp)
  (symbol? exp))

(#%provide type
           contents
           assertion-to-be-added?
           add-assertion-body
           var?
           constant-symbol?)
