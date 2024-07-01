#lang sicp
(#%require "../predicates.rkt")
(#%require "../core/binding.rkt")

(define (instantiate exp frame
          unbound-var-handler)
  (define (copy exp)
    (cond
      ;; 如果是一个变量，我们就尝试从 frame 中寻找对应的 binding
      [(var? exp)
       (let ([binding (binding-in-frame exp frame)])
         ;; 如果找到，就继续尝试 copy 其结果 (因为 binding 有可能结果也是变量作为引用)
         (if binding
             (copy (binding-value binding))
             (unbound-var-handler exp frame)))]
      ;; 如果是 list，则递归一个一个处理并连接
      ;; 理论上, exp frame 应该是一个 binding list
      ;; 我们一个一个提出 bidnding 进行数据实例化
      [(pair? exp) (cons (copy (car exp)) (copy (cdr exp)))]
      ;; 其他情况下，直接返回原始值
      [else exp]))
  (copy exp))

(#%provide instantiate)
