#lang sicp

(#%require "../evaluator/utils.rkt")
(#%require
 "../../../common/data/conventional-interface.rkt")

;; rewrite env operations

(define (enclosing-environment env)
  (cdr env))
(define (first-frame env)
  (car env))
(define the-empty-enviroment '())

;; it doesn't restruct the length of vars and vals
;; we let `extend-environment` to check this
(define (make-frame vars vals)
  (map cons vars vals))
(define (add-binding-to-frame! var val frame)
  (cons (cons var val) frame))

;; find value of a variable inside an environment
(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-enviroment)
        (error "Unbound variable" var)
        (let ([ret (assoc var (first-frame env))])
          (if ret
              (cdr ret)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

;; create a new environment with a new frame in which
;; all the variables are bound to their values
;; the returned new environment's outer environment is base-env
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (define-variable! var val env)
  (let* ([frame (first-frame env)] [ret (assoc var frame)])
    (if ret
        (set-cdr! ret val)
        (add-binding-to-frame! var val frame))))

;; almost the same as lookup
(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-enviroment)
        (error "Unbound variable -- SET!" var)
        (let ([ret (assoc var (first-frame env))])
          (if ret
              (set-cdr! ret val)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(#%provide lookup-variable-value
           extend-environment
           define-variable!
           set-variable-value!)
