#lang sicp

(define (make-operation-exp exp machine labels operations)
  (let ([op (lookup-prim (operation-exp-op exp) operations)]
        [aprocs
         (map (lambda (e)
                (if (or (register-exp? e) (constant-exp? e))
                    (make-primitive-exp e machine labels)
                    (error "Invalid Argument -- ASSEMBLE"
                           e)))
              (operation-exp-operands exp))])
    ;; 将 operands 全部转化为待执行的基本过程, 包在一个大的执行过程里面，需要时再执行
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))
