#lang sicp

;; evaluator interal data structure about environment

;; environment is a list of frame
;; cdr is its enclosing environment

;; outer environment
(define (enclosing-environment env)
  (cdr env))
(define (first-frame env)
  (car env))
(define the-empty-enviroment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame)
  (car frame))
(define (frame-values frame)
  (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; find value of a variable inside an environment
(define (lookup-variable-value var env)
  ;; recursive find var's val in env
  ;; if not founded, try to search enclosing env
  (define (env-loop env)
    (define (scan vars vals)
      (cond
        [(null? vars)
         (env-loop (enclosing-environment env))]
        [(eq? var (car vars)) (car vals)]
        [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-enviroment)
        (error "Unbound variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
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
  (let ([frame (first-frame env)])
    (define (scan vars vals)
      (cond
        [(null? vars) (add-binding-to-frame! var val frame)]
        [(eq? var (car vars)) (set-car! vals val)]
        [else (scan (cdr vars) (cdr vals))]))
    (scan (frame-variables frame) (frame-values frame))))

;; almost the same as lookup
(define (set-variable-value! var value env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond
        [(null? vars)
         (env-loop (enclosing-environment env))]
        [(eq? var (car vars)) (set-car! vals val)]
        [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-enviroment)
        (error "Unbound variable -- SET!" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame))))
    (env-loop env)))

(#%provide lookup-variable-value
           extend-environment
           define-variable!
           set-variable-value!)
