#lang sicp

;; 目标: 不再需要用户提供有什么寄存器， 而是可以考虑
;; 1. 通过预先扫描 controller 序列判断有哪些寄存器
;; 2. 在 assemble 指令的时候， 遇到寄存器时才进行分配

;; 2 的做法明显要简单得多， 我们采用 2

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
              ;; 如果没有目标寄存器，先分配再查询并返回
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
