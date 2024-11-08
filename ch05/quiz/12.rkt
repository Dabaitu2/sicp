#lang sicp

;; 因为有很多玩意的读写方式都是相同的， 我们写两个专门的过程作为数据构造器来处理
;; 1. 处理简单的 symbol 去重
(define (make-unique-set)
  (let ([data '()])
    (define (update item)
      (if (null? data)
          (list item)
          (if (not (member item data))
              (set-cdr! data (cons item (cdr data))))))
    (define (dispatch message)
      (cond
        [(eq? message 'update)
         (lambda (item) (update item))]
        [(eq? message 'get) data]
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
            (append data
                    (list (cons key
                                (((make-unique-set) 'update)
                                 value))))
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
        [(eq? message 'get) data]
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
        [(eq? message 'get-sources) (get-data sources)]))
    dispatch))

(define (make-execution-procedure inst
                                  labels
                                  machine
                                  pc
                                  flag
                                  stack
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
    [(eq? (car inst) 'save)
     (make-save inst machine stack pc)]
    [(eq? (car inst) 'restore)
     (make-restore inst machine stack pc)]
    [(eq? (car inst) 'perform)
     (make-perform inst machine labels ops pc)]
    [else
     (error "Unkown instruction type: ASSEMBLE" inst)]))

(define (make-save inst machine stacks pc)
  (let ([reg-name (stack-inst-reg-name inst)])
    ;; from quiz 5.12 更新使用堆栈的寄存器表
    ((machine 'update-stack-regs) reg-name)
    (let ([reg (get-register machine reg-name)])
      (lambda ()
        (push (get-stack reg-name stacks)
              (cons reg-name (get-contents reg)))
        (advance-pc pc)))))

(define (make-restore inst machine stacks pc)
  (let ([target-reg-name (stack-inst-reg-name inst)])
    ;; from quiz 5.12 更新使用堆栈的寄存器表
    ((machine 'update-stack-regs) reg-name)
    (let ([reg (get-register machine target-reg-name)])
      (lambda ()
        (set-contents! reg
                       (pop (get-stack target-reg-name
                                       stacks)))
        (advance-pc pc)))))

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

(define (make-assign inst machine labels operations pc)
  (let ([target (get-register machine
                              (assign-reg-name inst))]
        [value-exp (assign-value-exp inst)])
    ;; 更新使用过 assign 的寄存器表, 以及这个寄存器的赋值来源指令
    ((machine 'update-sources) (assign-reg-name insts) inst)
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

(define (make-new-machine)
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stacks '()]
        [the-instruction-sequence '()]
        [data-components (install-data-components)])
    ;; 初始化单个 stack => 初始化 map 中的所有 stack
    (let ([the-ops (list (list 'initialize-stack
                               (lambda ()
                                 (map (lambda (stack)
                                        (stack 'initialize))
                                      stacks))))]
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
          [(eq? message 'stack) stacks]
          [(eq? message 'operations) the-ops]
          [(eq? message 'update-insts-groups)]
          ;; from quiz 5.12 将后续处理 fallback 到 data-component 去做，即使找不到，错误会由内层抛出
          [else (data-components message)]))
      dispatch)))
