#lang sicp

(#%require
 "../../../common/data/conventional-interface.rkt")
(#%require "../evaluator/special-forms/definition.rkt")
(#%require "../evaluator/derived-forms/let.rkt")

(define (enclosing-environment env)
  (cdr env))
(define (first-frame env)
  (car env))
(define the-empty-enviroment '())

(define (look-binding-in-frame var env find-proc next-proc)
  (let* ([frame (first-frame env)] [ret (assoc var frame)])
    (if ret (find-proc ret) (next-proc var env frame))))

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
  (env-loop
   env
   var
   (lambda (ret)
     (let ([val (cdr ret)])
       (if (eq? val '*unassigned*)
           (error
            "Unassigned Variable -- LOOKUP-VARIABLE-VALUE"
            (car ret))
           val)))))

;; filter out internal definition
;; and wrap the data inside lambda
(define (scan-out-defines proc-body)
  (let ([result
         (fold-right
          (lambda (cur acc)
            (if (definition? cur)
                ;; 构造变量提升
                (let ([def-var
                        (list (definition-variable cur)
                              ''*unassigned)]
                      ;; 构造变量赋值用于占位
                      [def-set
                        (list 'set
                              (definition-variable cur)
                              (definition-value cur))])
                  ;; 这里没有把顺序倒回去，因为对于 binding / set! 定义而言这应该不重要
                  (list (cons def-var (car acc))
                        (cons def-set (cadr acc))))
                ;; 如果非内部定义，就直接原样放回去
                (list (car acc) (cons cur (cadr acc)))))
          (list '() '())
          proc-body)])
    (let ([bindings (car result)] [body (cadr result)])
      (if (null? bindings) body (make-let bindings body)))))


;; output:
;; 
;; ;;; M-Eval input:
;;; (define (f x)
;;;   (define (even? n)
;;;     (if (= n 0)
;;;         true
;;;         (odd? (- n 1))))
;;;   (define (odd? n)
;;;     (if (= n 0)
;;;         false)
;;;     (even? (- n 1)))
;;;   (odd? x) 
;;;   )
;;; ;;; M-Eval value
;;; #<void>
;;; 
;;; ;;; M-Eval input:
;;; f
;;; 
;;; ;;; M-Eval value
;;; (compund-procedure (x) (let ((even? '*unassigned) (odd? '*unassigned)) (set even? (lambda (n) (if (= n 0) true (odd? (- n 1))))) (set odd? (lambda (n) (if (= n 0) false) (even? (- n 1)))) (odd? x)) <procedure-env>)
