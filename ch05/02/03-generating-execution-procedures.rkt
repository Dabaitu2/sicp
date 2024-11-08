#lang sicp

;; Generating Execution Procedures for Instructions

;; 根据我们之前定义的语法(5.1.5)，按照传入的指令进行实际的执行过程的生成
;; 这里跟
(define (make-execution-procedure inst
                                  labels
                                  machine
                                  pc
                                  flag
                                  stack
                                  ops)
  (cond
    [(eq? (car inst) 'assign)
     (make-assign inst machine labels ops pc)]
    [(eq? (car inst) 'test)
     (make-test inst machine labels ops pc)]
    [(eq? (car inst) 'branch)
     (make-branch inst machine labels flag pc)]
    [(eq? (car inst) 'goto)
     (make-goto inst machine labels pc)]
    [(eq? (car inst) 'save)
     (make-save inst machine stack pc)]
    [(eq? (car inst) 'restore)
     (make-restore inst machine stack pc)]
    [(eq? (car inst) 'perform)
     (make-peform inst machine labels ops pc)]
    [else
     (error "Unkown instruction type: ASSEMBLE" inst)]))

;; =================== Assign ===================
(define (make-assign inst machine labels operations pc)
  (let ([target (get-register machine
                              (assign-reg-name inst))]
        [value-exp (assign-value-exp inst)])
    (let ([value-proc (if (operation-exp? value-exp)
                          (make-operation-exp value-exp
                                              machine
                                              labels
                                              operations)
                          (make-primitive-exp
                           (car value-exp)
                           machine
                           labels))])
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

;; 从 assign instruction 中获取 register 的名字
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

;; 从 assign instruction 中获取 value-exp
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; =================== Test ===================
;; test 将检测的结果设置给 flag 寄存器， 后续 branch 可以根据 flag 的结果决定要不要跳转
(define (make-test inst machine labels operations flag pc)
  (let ([condition (test-condition test)])
    ;; test 的必须是操作, 否则报错
    (if (operation-exp? condition)
        (let ([condition-proc (make-operation-exp
                               condition
                               machine
                               labels
                               operations)])
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;; =================== Branch ===================
(define (make-branch inst machine labels flag pc)
  (let ([dest (branch-dest inst)])
    (if (label-exp? dest)
        (let ([insts (lookup-label labels
                                   (label-exp-label dest))])
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ([dest (goto-dest inst)])
    (cond
      [(lable-exp? dest)
       (let ([insts (lookup-label lables
                                  (label-exp-label dest))])
         (lambda () (set-contents! pc insts)))]
      [(register-exp? dest)
       (let ([reg (get-register machine
                                (register-exp-reg dest))])
         (lambda () (set-contents! pc (get-contents reg))))]
      [else
       (error "Bad GOTO instruction -- ASSEMBLE" inst)])))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; ==================== Stack Related Instructions ====================
(define (make-save inst machine stack pc)
  (let ([reg (get-register machine
                           (stack-inst-reg-name inst))])
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ([reg (get-register machine
                           (stack-inst-reg-name inst))])
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;; ===================== Perform Related Instructions =====================
(define (make-perform inst machine labels operations pc)
  (let ([action (perform-action inst)])
    (if (operation-exp? action)
        (let ([action-proc (make-operation-exp action
                                               machine
                                               labels
                                               opertaions)])
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFROM instruction -- ASSEMBLE"
               inst))))

(define (perform-action perform-instruction)
  (cadr perform-instruction))

;; ===================== 基本操作 =====================
;; 获取一个寄存器 / label 或 const 表达式的值是最基本的操作
(define (make-primitive-exp exp machine labels)
  (cond
    [(constant-exp? exp)
     (let ([c (constant-exp-value exp)]) (lambda () c))]
    [(label-exp? exp)
     (let ([insts (lookup-label labels
                                (label-exp-label exp))])
       (lambda () insts))]
    [(resgiter-exp? exp)
     (let ([r (get-register machine (register-exp-rg exp))])
       (lambda () (get-contents r)))]
    [else
     (error "Unknown expression type -- ASSEMBLE" exp)]))

;; ==================== Selectors & Predicates ====================
(define (tagged-list? exp tag)
  (if (pair? exp) (eq? (car exp) tag) false))

(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (constant-exp-value exp)
  (cadr exp))
(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label)
  (cadr exp))

;; ==================== 处理 Operations ====================
(define (make-operation-exp exp machine labels operations)
  (let ([op (lookup-prim (operation-exp-op exp) operations)]
        [aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))])
    ;; 将 operands 全部转化为待执行的基本过程, 包在一个大的执行过程里面，需要时再执行
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ([val (assoc symbol operations)])
    (if val
        (cadr val)
        (error "Unknown operations -- ASSEMBLE" symbol))))