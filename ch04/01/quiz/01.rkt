#lang sicp

(#%require "../evaluator/application.rkt")
(#%require "../evaluator/evaln.rkt")

;; 始终优先求值最左边的元素
(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      ;; 先求值最左边的
      (let ([first-value (evaln (first-operand exps) env)])
        (cons (first-value (list-of-values-left-to-right
                            (rest-operands exps)
                            env))))))

;; 始终优先求值最右边的元素，也就是直到 rest 没有元素才开始求值
(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      ;; 先求值剩下的,  再求值第一个
      (let ([other-values (list-of-values-right-to-left
                           (rest-operands exps) env)]
            [first-value (evaln (first-operand exps) env)])
        (cons first-value other-values))))
