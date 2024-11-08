#lang sicp

;; (save y)
;; (save x)
;; (restore y)

;; 行为 1. 不管 save 的是什么变量， 都将最后一个变量的值吐给 y, 这是原始行为
;; 行为 2. 如果最后一个值是 y save 的， 就吐， 否则报错
;; 行为 3. 从堆栈中找到最后一个 y save 的值, 吐出来。

;; 我们先实现行为 2
;; 更改 save 使其不仅存入 value 还要存入寄存器的名字
(define (make-save inst machine stack pc)
  (let ([reg-name (stack-inst-reg-name inst)])
    (let ([reg (get-register machine reg-name)])
      (lambda ()
        (push stack (cons reg-name (get-contents reg)))
        (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let ([target-reg-name (stack-inst-reg-name inst)])
    (let ([reg (get-register machine target-reg-name)])
      (lambda ()
        (let ([stack-record (pop stack)])
          (let ([stack-reg-name (car stack-record)]
                [stack-reg-value (cdr stack-record)])
            (if (eq? stack-reg-name target-reg-name)
                (begin
                  (set-contents! reg stack-reg-value)
                  (advance-pc pc))
                (error
                 "Restoring Reg Name is not match -- RESTORE"
                 target-reg-name))))))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;; 接下来实现行为3， 这应该是最完备的
;; 1. 我们需要为每个寄存器都关联一个堆栈
;; 2. 需要修改 initialize-stack 让它初始化所有的寄存器 stack
;; 这里有两种办法：
;; 1. 用闭包在寄存器过程中存储堆栈, 但这样改动比较大，会需要给 register 加很多方法
;; 2. 在 make-new-machine 里开一个 map 去关联 reg name 和 stack, 这个改动比较小
;; 我们用方法 2
(define (make-save inst machine stacks pc)
  (let ([reg-name (stack-inst-reg-name inst)])
    (let ([reg (get-register machine reg-name)])
      (lambda ()
        (push (get-stack reg-name stacks)
              (cons reg-name (get-contents reg)))
        (advance-pc pc)))))

(define (make-restore inst machine stacks pc)
  (let ([target-reg-name (stack-inst-reg-name inst)])
    (let ([reg (get-register machine target-reg-name)])
      (lambda ()
        (set-contents! reg
                       (pop (get-stack target-reg-name
                                       stacks)))
        (advance-pc pc)))))

(define (get-stack reg-name stacks)
  (let ([stack (assoc reg-name stacks)])
    (if stack
        (cadr stack)
        (error "Can't find stack for reg:" reg-name))))

(define (make-new-machine)
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stacks '()]
        [the-instruction-sequence '()])
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
          [else
           (error "Unknown request -- MACHINE" message)]))
      dispatch)))
