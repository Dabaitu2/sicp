#lang racket
(#%require "./tag-tools.rkt")
(#%require "./env.rkt")

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  ;; 为了维持闭包性质，需要加 tag, 而 获取的 xy 因为已经被 apply-generic 处理了
  ;; 所以是不带 tag 的 content
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

;; 这里不能使用 apply-generic 是因为这里的场景下
;; 我们是从原始值生成一个带 tag 的数据
;; 而 apply-generic 包装后处理的已经是带 tag 的数据了
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(#%provide install-scheme-number-package make-scheme-number)
