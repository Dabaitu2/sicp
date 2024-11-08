#lang sicp

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
(define (update-insts! insts label machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stack (machine 'stack)]
        [ops (machine 'operations)])
    ;; 在 recevie 调用之前， 参数就已经通过 (cons (make-instruction)) 或者 (cons (make-label-entry)) 插入了新的只包含 instruction 或者 label 的元素
    ;; 在这里， 我们要扩展这个 instruction, 产生一个 execution procedure 使之真的可以运行
    (for-each (lambda (inst)
                (set-instruction-execution-proc!
                 inst
                 (make-execution-procedure
                  (instruction-text inst)
                  labels
                  machine
                  pc
                  flag
                  stack
                  ops))))))

;; instruction 的 text 其实并不会被使用， 但是对于 debug 可能很有用
(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car insts))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

;; =================== Label Entry ====================
(define (make-label-entry label-name insts)
  (const label-name insts))

(define (lookup-label labels label-name)
  (let ([val (assoc label-name labels)])
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))
