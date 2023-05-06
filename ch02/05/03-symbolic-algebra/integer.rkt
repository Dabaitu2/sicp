#lang racket
(#%require "./tag-tools.rkt")
(#%require "./env.rkt")

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  ;; 为了维持闭包性质，需要加 tag, 而 获取的 xy 因为已经被 apply-generic 处理了
  ;; 所以是不带 tag 的 content
  (put 'add '(integer integer) +)
  (put 'sub '(integer integer) -)
  (put 'mul '(integer integer) *)
  (put 'div '(integer integer) /)
  (put 'negate '(integer) (lambda (x) (tag (- x))))
  (put 'cosine '(real) (lambda (x) (tag (round (cos x)))))
  (put 'sine '(real) (lambda (x) (tag (round (sin x)))))
  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer) (lambda (x) (= x 0)))
  (put 'make
       'integer
       (lambda (x)
         (if (number? x)
             (tag (round x))
             (error "arg is not number -- make-integer"
                    x))))
  'done)

;; 这里不能使用 apply-generic 是因为这里的场景下
;; 我们是从原始值生成一个带 tag 的数据
;; 而 apply-generic 包装后处理的已经是带 tag 的数据了
(define (make-integer n)
  ((get 'make 'integer) n))

(#%provide install-integer-package make-integer)
