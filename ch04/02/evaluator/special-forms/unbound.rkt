#lang sicp

(#%require "../utils.rkt")
(#%require "../env.rkt")

;; 处理 unbound 特殊形式
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

(#%provide unbound?
           make-unbound!
           unbound-var
           eval-unbound!)
