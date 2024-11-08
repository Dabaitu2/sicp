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
        [eq?
         message
         'set]
        [lambda
         (value)
         (set! contents value)])
      (else (error "Unknown request -- REGISTER" message)))
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
        [else (error "Unknown request : STACK" message)]))))

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
          [register-rable (list (list 'pc pc)
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
          (if (null insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      ;; 消息传递
      (define (dispatch (message))
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
