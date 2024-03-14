#lang sicp
(#%require "../utils.rkt")
(#%require "../evaln.rkt")

;; 判断一个表达式是不是引号表达式其实就是看一个 list 的开头是不是某个特定符号
;; 这也是 lisp 比较优雅的地方，语法结构非常简单
;; 所有的东西其实都可以看作是 pair (cons) 构成的, 而不像其他语言那样要针对不同的语法结构写复杂的 parser
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp env)
  (let ([content (cadr exp)])
    (if (pair? content)
        (eval (make-lazy-list content) env)
        content)))

;; 将 quoted list 转化为调用我们预先建设的一般过程 cons 
(define (make-lazy-list exp)
  (if (null? exp)
      '()
      (list 'cons (car exp) (make-lazy-list (cdr exp)))))

(#%provide quoted? text-of-quotation)
