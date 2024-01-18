#lang sicp

(#%require "../evaluator/utils.rkt")
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
(define (add-binding-to-frame! var val frame env)
  (if (null? frame)
      (set-car! env (list (cons var val)))
      (set-cdr! frame (cons (cons var val) (cdr frame)))))

;; create a new environment with a new frame in which
;; all the variables are bound to their values
;; the returned new environment's outer environment is base-env
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (look-binding-in-frame var env find-proc next-proc)
  (let* ([frame (first-frame env)] [ret (assoc var frame)])
    (if ret (find-proc ret) (next-proc var env frame))))

(define (define-variable! var val env)
  (look-binding-in-frame
   var
   env
   (lambda (ret) (set-cdr! ret val))
   (lambda (var env frame)
     (add-binding-to-frame! var val frame env))))

(define (env-loop env var proc)
  (if (eq? env the-empty-enviroment)
      (error "Unbound variable -- SET!" var)
      (look-binding-in-frame
       var
       env
       (lambda (ret) (proc ret))
       (lambda (var env _frame)
         (env-loop (enclosing-environment env) var proc)))))

(define (lookup-variable-value var env)
  (env-loop env var (lambda (ret) (cdr ret))))

(define (set-variable-value! var val env)
  (env-loop env var (lambda (ret) (set-cdr! ret val))))

(#%provide lookup-variable-value
           extend-environment
           define-variable!
           set-variable-value!
           look-binding-in-frame
           the-empty-enviroment)
