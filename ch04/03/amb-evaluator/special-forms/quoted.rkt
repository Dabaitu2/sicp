#lang sicp
(#%require "../utils.rkt")

;; 判断一个表达式是不是引号表达式其实就是看一个 list 的开头是不是某个特定符号
;; 这也是 lisp 比较优雅的地方，语法结构非常简单
;; 所有的东西其实都可以看作是 pair (cons) 构成的, 而不像其他语言那样要针对不同的语法结构写复杂的 parser
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (analyze-quoted exp)
  (let ([qval (text-of-quotation exp)])
    (lambda (env succeed fail) (succeed qval fail))))

(#%provide quoted? text-of-quotation analyze-quoted)
