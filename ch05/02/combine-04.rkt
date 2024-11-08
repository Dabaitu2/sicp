#lang sicp

;; 机器模型 实现 => 04 章为止
;; 增加了 quiz 中提到的一些改动
;; 不再传递寄存器的名字， 不再预先分配
(define (make-machine ops controller-text)
  (let ([machine (make-new-machine)])
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
        [(eq? message 'name) name]
        [else
         (error "Unknown request -- REGISTER" message)]))
    dispatch))

;; 访问寄存器
(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (get-name register)
  (register 'name))

;; =================== Stack ===================
;; 修改 stack 使其能够记录 寄存器堆栈深度
(define (make-stack)
  (let ([s '()]
        [number-pushes 0]
        [max-depth 0]
        [current-depth 0])
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ([top (car s)])
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes
                     '=
                     number-pushes
                     'maximum-depth
                     '=
                     max-depth)))
    (define (dispatch message)
      (cond
        [(eq? message 'push) push]
        [(eq? message 'pop) (pop)]
        [(eq? message 'initialize) (initialize)]
        [(eq? message 'print-statistics) (print-statistics)]
        [else (error "Unknown request : STACK" message)]))
    dispatch))

(define (pop stack)
  (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

;; =================== make-new-machine ===================
;; make-new-machine 也增加一个打印堆栈统计数据的操作
(define (make-new-machine)
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stacks '()]
        [the-instruction-sequence '()]
        [data-components (install-data-components)])
    ;; 初始化单个 stack => 初始化 map 中的所有 stack
    (let ([the-ops
           (list
            (list 'initialize-stack
                  (lambda ()
                    (map (lambda (stack)
                           (stack ''initializeinitialize))
                         stacks)))
            ;; 这里和书本上的有一定区别， 我们持续采用 quiz 11 提到的一个寄存器一个堆栈来处理
            (list 'print-stack-statistics
                  (lambda (reg-name)
                    (newline)
                    (display "reg-name: ")
                    (display reg-name)
                    (newline)
                    (let ([stack (get-stack reg-name
                                            stacks)])
                      (stack 'print-statistics)))))]
          [register-table (list (list 'pc pc)
                                (list 'flag flag))])
      ;; 分配新的寄存器， 查找寄存器的值
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (begin
              (set! register-table
                    (cons (list name (make-register name))
                          register-table))
              (set! stacks
                    (cons (list name (make-stack))
                          stacks)))))
      (define (lookup-register name)
        (let ([val (assoc name register-table)])
          (if val
              (cadr val)
              (begin
                (allocate-register name)
                (lookup-register name)))))
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
          [(eq? message 'stack) stacks]
          [(eq? message 'operations) the-ops]
          ;; from quiz 5.12 将后续处理 fallback 到 data-component 去做，即使找不到，错误会由内层抛出
          [else (data-components message)]))
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
                        ;; label 后面的部分都认为是 label 所关联的指令代码
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
                                  ops)
  ;; from quiz 5.12 更新指令表
  ((machine 'update-insts-groups) (car inst) inst)
  (cond
    [(eq? (car inst) 'inc) (make-inc inst machine pc)]
    [(eq? (car inst) 'assign)
     (make-assign inst machine labels ops pc)]
    [(eq? (car inst) 'test)
     (make-test inst machine labels ops flag pc)]
    [(eq? (car inst) 'branch)
     (make-branch inst machine labels flag pc)]
    [(eq? (car inst) 'goto)
     (make-goto inst machine labels pc)]
    [(eq? (car inst) 'save) (make-save inst machine pc)]
    [(eq? (car inst) 'restore)
     (make-restore inst machine pc)]
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
    ;; 更新使用过 assign 的寄存器表, 以及这个寄存器的赋值来源指令
    ((machine 'update-sources) (assign-reg-name inst) inst)
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
        ;; (display "[1]ready to set register: ")
        ;; (display (target 'name))
        ;; (display " to value: ")
        ;; (display (value-proc))
        ;; (newline)
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
       (let ([reg-name (register-exp-reg dest)])
         ;; from quiz 5.12 更新使用 goto 会跳转到的寄存器入口表
         ((machine 'update-entry-points) reg-name)
         (let ([reg (get-register machine reg-name)])
           (lambda ()
             (set-contents! pc (get-contents reg)))))]
      [else
       (error "Bad GOTO instruction -- ASSEMBLE" inst)])))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; ==================== Stack Related Instructions ====================
;; 不再从外界传入 stack, 因为这个 stack 是会在 update-insts! 过程中被动态更新的
(define (make-save inst machine pc)
  (let ([stacks (machine 'stack)])
    (let ([reg-name (stack-inst-reg-name inst)])
      ;; from quiz 5.12 更新使用堆栈的寄存器表
      ((machine 'update-stack-regs) reg-name)
      (let ([reg (get-register machine reg-name)])
        (lambda ()
          (push (get-stack reg-name stacks)
                (cons reg-name (get-contents reg)))
          (advance-pc pc))))))

(define (make-restore inst machine pc)
  (let ([stacks (machine 'stack)])
    (let ([target-reg-name (stack-inst-reg-name inst)])
      ;; from quiz 5.12 更新使用堆栈的寄存器表
      ((machine 'update-stack-regs) target-reg-name)
      (let ([reg (get-register machine target-reg-name)])
        (lambda ()
          ;; save 的时候会将 reg-name 和 reg-value 一起保存到堆栈中
          ;; 因此 pop 取的时候要注意
          (let ([reg-value
                 (cdr (pop (get-stack target-reg-name
                                      stacks)))])

            (set-contents! reg reg-value)
            (advance-pc pc)))))))

(define (get-stack reg-name stacks)
  (let ([stack (assoc reg-name stacks)])
    (if stack
        (cadr stack)
        (error "Can't find stack for reg:" reg-name))))

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
  (cdr perform-instruction))

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
       (lambda ()
         ;; TODO: 干掉这坨
         ;; (display "reg-name: ")
         ;; (display (register-exp-reg exp))
         ;; (display ", value: ")
         ;; (display (get-contents r))
         ;; (newline)
         (get-contents r)))]
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

;; 因为有很多玩意的读写方式都是相同的， 我们写两个专门的过程作为数据构造器来处理
;; 1. 处理简单的 symbol 去重
(define (make-unique-set)
  (let ([data '()])
    (define (update item)
      (if (null? data)
          (set! data (list item))
          (if (not (member item data))
              (set-cdr! data (cons item (cdr data))))))
    (define (dispatch message)
      (cond
        [(eq? message 'update)
         (lambda (item) (update item))]
        [(eq? message 'get)
         (begin
           (display data)
           (newline))]
        [else
         (error "Unknown Request -- DATA COMPONENT"
                message)]))
    dispatch))

;; 2. 处理稍微复杂的类似 map<key, set> 的数据
(define (make-unique-map-set)
  (let ([data '()])
    (define (update key value)
      (define (iter map-sets)
        (if (null? map-sets)
            (let ([new-set (make-unique-set)])
              ((new-set 'update) value)
              (set! data
                    (append (list (cons key new-set))
                            data)))
            (let ([cur-map-set (car map-sets)])
              (let ([cur-key (car cur-map-set)]
                    [cur-set (cdr cur-map-set)])
                (if (eq? key cur-key)
                    ((cur-set 'update) value)
                    (iter (cdr map-sets)))))))
      (iter data))
    (define (dispatch message)
      (cond
        [(eq? message 'update)
         (lambda (key value) (update key value))]
        [(eq? message 'get)
         (for-each (lambda (map-set)
                     (display (car map-set))
                     (display ": ")
                     ((cdr map-set) 'get))
                   data)]
        [else
         (error "Unknown Request -- DATA GROUPS" message)]))
    dispatch))

;; modify the assemble procedure to store and provide interfaces
;; to let the user access the data paths
;; 为了实现目标
;; 1. 初始化 new-machine 时我们要预先建设几个变量来存储这些东西
;; 2. machine 所构造的过程应该提供一些工具方法来更新这些东西，使之满足题目要求, 例如去重等
;; 3. 在 assemble 过程中，我们利用 machine 提供的工具方法来做这些事情
;; 这里传入的 insts 应该是 一个 (cons inst-text execution-proc), 对应一个指令的过程
;; inst-text 是这个指令的文本，execution-proc 是这个指令的执行过程, 注意指令不是简单的 add / goto, 这个是 inst-type
;; 指令文本是完整的 (add a b) 这种文本
;; 或者是一个 (cons label-text label-inst)
(define (install-data-components)
  ;; insts-groups 例子:
  ;; (list
  ;;   (cons 'assign '((assign x y) (assign z t)))
  ;;   (cons 'goto '((goto x) (goto b)))
  ;;   ...
  ;; )
  (let ([insts-groups (make-unique-map-set)]
        ;; entry-points 是 goto 所指向的寄存器的集合
        ;; 例如 (goto (reg continue))
        ;; 在这里就会存储 continue
        [entry-points (make-unique-set)]
        ;; 这里不用存储具体是 save 还是 restore, 只要是使用过这两者之一就存储
        [stack-regs (make-unique-set)]
        [sources (make-unique-map-set)])
    (define (get-data data-component)
      (data-component 'get))
    (define (dispatch message)
      (cond
        [(eq? message 'update-insts-groups)
         (lambda (inst-type inst-text)
           ((insts-groups 'update) inst-type inst-text))]
        [(eq? message 'update-entry-points)
         (lambda (reg-name)
           ((entry-points 'update) reg-name))]
        [(eq? message 'update-stack-regs)
         (lambda (reg-name)
           ((stack-regs 'update) reg-name))]
        [(eq? message 'update-sources)
         (lambda (reg-name source-text)
           ((sources 'update) reg-name source-text))]
        [(eq? message 'get-insts-groups)
         (get-data insts-groups)]
        [(eq? message 'get-entry-points)
         (get-data entry-points)]
        [(eq? message 'get-stack-regs)
         (get-data stack-regs)]
        [(eq? message 'get-sources) (get-data sources)]
        [else
         (error "Unknown Request -- ASSEMBLE" message)]))
    dispatch))

(#%provide make-machine
           set-register-contents!
           get-register-contents
           start)
