#lang sicp

;; unless 可以实现为一种派生形式
;; 并且这种形式是可以正常运行的
;; 这是因为此时的 unless 是一种语法结构而非一种过程
;; 我们不会按照过程的执行方式去求解它，也就是说
;; 我们不会先去求值参数，再应用过程, 而是将其转化为适当时候再求值 (本质就是惰性求值)
;; 从而规避了应用序本可能出现的问题

(define (unless? exp)
  (tagged-list? exp 'unless))

(define (unless-clause exp)
  (cdr exp))
(define (unless-condition clause)
  (car clause))
(define (unless-usual-value clause)
  (cadr clause))
(define (unless-exceptional-value clause)
  (caddr clause))

(define (unless->if exp)
  (let ([clause (unless-clause exp)])
    (make-if (unless-condition clause)
             (unless-exceptional-value clause)
             (unless-usual-value clause))))

(define (analyze-unless exp)
  (lambda (env) ((analyze (unless->if exp)) env)))

;; 然而，正如 Alyssa 的说法，将 unless 实现为一个语法结构，会导致 unless
;; 无法被作为一等公民传入
;; 正如我们不能把 if 当成参数传递给其他高阶过程一样
;; 因为 unless 必须和它对应的语法结构一起出现 (一个 (cons unless ...))
;; 而不可以仅仅传入这个标识 (一个 symbol)
;; 否则，它会被命中为 variable 变量，而 env 中是没有这个所谓的 unless 变量的
