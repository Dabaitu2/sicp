#lang sicp

(define (make-machine register-names ops controller-text)
  ;; make-new-machine 创造出一个空白的机器，具有寄存器机器模型里都需要的公共部分
  ;; machine 中指令的调用采用信息传递方案
  (let ([machine (make-new-machine)])
    ;; 将寄存器分配进去
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ;; 安装操作
    ((machine 'install-opeartions) ops)
    ;; 安装指令序列
    ((machine 'install-instruction-sequence)
     ;; 通过 汇编程序进行代码变换/组装, 产生新的(编译后的)指令序列
     ;; 这样这个指令才能够被简单的 machine 执行
     (assemble controller-text machine))
    ;; 最后返回修改后的机器
    machine))

;; =================== Register ===================
;; 寄存器可以表示成一个带有局部状态的过程
(define (make-register name)
  (let ([contents '*unassigned*])
    (define (dispatch message)
      (cond
        [(eq? message 'get) contents]
        [(eq? message 'set)
         (lambda (value) (set! contents value))]
        [else
         (error "Unknown request -- REGISTER" message)]))
    dispatch))

;; 访问寄存器
(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; =================== Stack ===================
(define (make-stack)
  (let ([s '()])
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ([top (car s)])
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond
        [(eq? message 'push) push]
        [(eq? message 'pop) (pop)]
        [(eq? message 'initialize) (initialize)]
        [else (error "Unknown request : STACK" message)]))
    dispatch))

(define (pop stack)
  (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

;; =================== make-new-machine ===================
(define (make-new-machine)
  ;; 初始化内部寄存器， 堆栈和指令序列
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stack (make-stack)]
        [the-instruction-sequence '()])
    (let ([the-ops (list (list 'initialize-stack
                               (lambda ()
                                 (stack 'initialize))))]
          [register-table (list (list 'pc pc)
                                (list 'flag flag))])
      ;; 分配新的寄存器， 查找寄存器的值
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table))))
      (define (lookup-register name)
        (let ([val (assoc name register-table)])
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      ;; 执行指令序列
      (define (execute)
        (let ([insts (get-contents pc)])
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      ;; 消息传递
      (define (dispatch message)
        (cond
          [(eq? message 'start)
           (set-contents! pc the-instruction-sequence)
           (execute)]
          [(eq? message 'install-instruction-sequence)
           (lambda (seq)
             (set! the-instruction-sequence seq))]
          [(eq? message 'allocate-register)
           allocate-register]
          [(eq? message 'get-resgitser) lookup-register]
          [(eq? message 'install-opeartions)
           (lambda (ops)
             (set! the-ops (append the-ops ops)))]
          [(eq? message 'stack) stack]
          [(eq? message 'operations) the-ops]
          [else
           (error "Unknown request -- MACHINE" message)]))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-resgitser) reg-name))

(define (assemble controller-text machine)
  ;; 1. extract-labels 根据 controller-text 构造出 initial instruction list 和 label 列表
  ;; 2. 通过对 controller-text 进行分析, 创建出 instaruction exection procedures 并且插入 initial 指令序列 (一开始估计是空的)
  (extract-labels
   controller-text
   ;; receive 接受两个参数，instruction list 和 label list
   ;; 它会将 machine 按照这两个参数进行更新， 并返回传入的 instruction list
   (lambda (insts labels)
     (update-insts! insts labels machine)
     insts)))

(define (extract-labels text receive)
  (if (null? text)
      ;; 如果 text 为空，经由外部的 update-insts! 将 machine 更新为空，并返回空的 instruction list
      ;; 这个时候才会触发第一次 receive (实际上已经被包装过 n 层)
      (receive '() '())
      (extract-labels
       (cdr text)
       ;; 包装 receive, 以便在调用 receive 时传入被扩展的 insts 或者 labels
       ;; 最终，最内层的 extract-labels 将会调用 (receive '() '())
       ;; 而此时的 receive 将会是下面的这个 lambda 的套娃版本,
       ;; 这其中将一个一个的从倒数第一个字符(存在环境闭包里)/数字开始进行对原始 instruction list / label list 进行扩展
       (lambda (insts labels)
         (let ([next-inst (car text)])
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               ;; 否则认为是指令的一部分
               ;; 就往指令序列中插入一个指令
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

;; 任何用户自定义的指令， 不过就是在操作堆栈, 寄存器, 操作符罢了
(define (update-insts! insts labels machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stack (machine 'stack)]
        [ops (machine 'operations)])
    ;; 在 recevie 调用之前， 参数就已经通过 (cons (make-instruction)) 或者 (cons (make-label-entry)) 插入了新的只包含 instruction 或者 label 的元素
    ;; 在这里， 我们要扩展这个 instruction, 产生一个 execution procedure 使之真的可以运行
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure (instruction-text inst)
                                  labels
                                  machine
                                  pc
                                  flag
                                  stack
                                  ops)))
     insts)))

;; instruction 的 text 其实并不会被使用， 但是对于 debug 可能很有用
(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

;; =================== Label Entry ====================
(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ([val (assoc label-name labels)])
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

(define (make-execution-procedure inst
                                  labels
                                  machine
                                  pc
                                  flag
                                  stack
                                  ops)
  (cond
    [(eq? (car inst) 'inc)
     (make-inc inst machine labels pc)]
    [(eq? (car inst) 'assign)
     (make-assign inst machine labels ops pc)]
    [(eq? (car inst) 'test)
     (make-test inst machine labels ops flag pc)]
    [(eq? (car inst) 'branch)
     (make-branch inst machine labels flag pc)]
    [(eq? (car inst) 'goto)
     (make-goto inst machine labels pc)]
    [(eq? (car inst) 'save)
     (make-save inst machine stack pc)]
    [(eq? (car inst) 'restore)
     (make-restore inst machine stack pc)]
    [(eq? (car inst) 'perform)
     (make-perform inst machine labels ops pc)]
    [else
     (error "Unkown instruction type: ASSEMBLE" inst)]))

;; ==================== Custom Register Syntax: Inc ====================
(define (make-inc inst machine pc)
  (let ([target (get-register machine
                              (inc-reg-source inst))])
    (lambda ()
      (set-contents! target (+ 1 (get-contents target)))
      (advance-pc pc))))

(define (inc-reg-source inst)
  (cadr inst))
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
  (let ([condition (test-condition inst)])
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
      [(label-exp? dest)
       (let ([insts (lookup-label labels
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
                                               operations)])
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
    [(register-exp? exp)
     (let ([r (get-register machine
                            (register-exp-reg exp))])
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
(define (label-exp-label exp)
  (cadr exp))

;; ==================== 处理 Operations ====================
(define (make-operation-exp exp machine labels operations)
  (let ([op (lookup-prim (operation-exp-op exp) operations)]
        [aprocs
         (map (lambda (e)
                ;; 不允许 label 作为操作数, from quiz 5.9
                (if (or (register-exp? e) (constant-exp? e))
                    (make-primitive-exp e machine labels)
                    (error "Invalid Argument -- ASSEMBLE"
                           e)))
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

(#%provide make-machine
           set-register-contents!
           get-register-contents
           start)