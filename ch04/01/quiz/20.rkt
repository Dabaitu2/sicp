#lang sicp
(#%require "../evaluator/utils.rkt")
(#%require "../evaluator/derived-forms/let.rkt")
(#%require
 "../../../common/data/conventional-interface.rkt")

;; racket 的 let 和后来抄它的 js 是一样的，内部正常情况下是不会有
;; 变量提升的
;; 但有时候我们想要递归声明
;; 因此 scheme 提供了 letrec 来实现递归声明
;;
;; a.
;; 实现 letrec
;; 它是 let 的派生，但是其中定义的 var 就都是同时建立的
;; 相当于显式的变量提升
;; 其 use case 如下

(define (f x)
  (letrec ([even? (lambda (n)
                    (if (= n 0) true (odd? (- n 1))))]
           [odd? (lambda (n)
                   (if (= n 0) false (even? (- n 1))))])
    (odd? x)))

(f 2)

(define (letrec? exp)
  (tagged-list? exp 'letrec))
(define (letrec-bindings clause)
  (car clause))
(define (letrec-body clause)
  (cdr clause))

(define (letrec->let exp)
  (let ([bindings (letrec-bindings exp)]
        [body (letrec-body exp)])
    (let ([result (fold-right
                   (lambda (cur acc)
                     (let ([let-var (list (caar cur)
                                          '*unassigned*)]
                           [let-set (list 'set
                                          (caar cur)
                                          (cdar cur))])
                       (list (cons let-var (car acc))
                             (cons let-set body))))
                   (list '() '())
                   bindings)])
      (let ([new-bindings (car result)]
            [new-body (cadr result)])
        (make-let new-bindings new-body)))))

;; b. louis 的想法为什么不严谨?
;; 假设我们的 代码从 letrec 变成 let
;; 在我们自己的 eval 实现中, 下面的代码无法正常实现
;;
;; (define (f x)
;;   (let ([even? (lambda (n)
;;                  (if (= n 0) true (odd? (- n 1))))]
;;         [odd? (lambda (n)
;;                 (if (= n 0) false (even? (- n 1))))])
;;     (odd? x)))
;; 
;; => (f 5)
;; => (((lambda (even? odd?) (odd? x)) 
;;       (lambda (n) (if (= n 0) true (odd? (- n 1))))
;;       (lambda (n) (if (= n 0) false (even? (- n 1))))) 
;;      5)
;; lisp 也是词法作用域
;; 这意味着, 我们再执行外侧的 (f x) 时，对应的 lambda 中的传入给 even? 和 odd? 的两个实际参数已经被求值
;; 而求值的结果是两个过程对象，他们的 base env 分别指向的时 (f x) 产生的 env 
;; 而不是内侧 (lambda) 表达式的 env，而在这个 env 中，even 和 odd 都还是未绑定的, 因此我们无法正常求值
;; 
;; 
;;                 ┌──────────────────┐
;;                 │ global frame     │
;;                 │                  │
;;                 │ (f 5)            │
;;                 └──────────────────┘
;; 
;;                  ┌────────────────────┐
;;                  │ (f 5) frame        │  (((lambda (even? odd?) (...))
;;                  │                    │      (lambda (n)..)
;;                  │ get value of lambda│      (lambda (n)..)) 5)
;;              ┌──►│ get value of 5     │
;;              │   │ apply 5 to evaluated lambda  ◄───────────────────┐
;;              │   └─────────────────────                             │
;;      base-env│                                                      │
;;              │                   ┌──────────────────┐               │
;;              │                   │ lambda frame     │               │
;;              │                   │                  │               │
;;              │                   │ even? -> lambda──┼──►┌───┬───┐   │base-env
;;        ┌───┬─┴─┬─◄───────────────┤ odd? -> lambda   │   │ . │ . ├───┘
;;        │ . │ . │                 └──────────────────┘   └─┬─┴───┘
;;        └─┬─┴───┘                                          │
;;          │                                                ▼
;;          ▼                                              params: n
;;         params: n                                       body: (if (= n ...)
;;         body: (if (= n ...)
;; 



