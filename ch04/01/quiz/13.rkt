#lang sicp

(#%require "../evaluator/utils.rkt")
(#%require "../evaluator/env.rkt")

;; achieve make-unbound! to unbound a binding
;;
;; if make-unbound! can be effective globally, that can easily cause unpredictable side effects
;; so I'm not preferring achieve it, instead, I just check the binding inside given environment

(define (unbound? exp)
  (tagged-list? exp 'unbound))

(define (make-unbound! var)
  (list 'unbound var))

(define (unbound-var exp)
  (cadr exp))

(define (eval-unbound! exp env)
  (look-binding-in-frame
   (unbound-var exp)
   env
   (lambda (ret) ((set-car! ret '()) (set-cdr! ret '())))
   ;; 不抛错，静默处理
   (lambda (_var _env _frame) '())))
