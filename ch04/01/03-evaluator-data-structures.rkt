#lang sicp


;; achive some internal data structure of evaluator
;; to indicate environment, represent true / false value ..


(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (not (eq? x true)))

(define (primitive-procedure? procedure)
  (display "TODO"))
(define (apply-primitive-procedure procedure arguments)
  (display "TODO"))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters procedure)
  (cadr p))
(define (procedure-body procedure)
  (caddr p))
(define (procedure-environment procedure)
  (cadddr p))


(define (enclosing-environment env)
  (cdr env))
(define (first-value env)
  (car env))
(define the-empty-enviroment '())

(define (make-frame variables values)
  (cons variables values))

;; var list + val list
;; ratrher than (var val) list
;; we will modify it in quiz 4.11
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
