#lang sicp

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
        [(eq? message
              'print-stack-statistics
              (print-statistics))]
        [else (error "Unknown request : STACK" message)]))
    dispatch))

;; make-new-machine 也增加一个打印堆栈统计数据的操作
;; 这里和教材上稍有不同, 我们还是采取了之前 quiz 中每个寄存器一个 stack 的方式
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
                           (stack 'initialize))
                         stacks)))
            ;; 这里和书本上的有一定区别， 我们持续采用 quiz 11 提到的一个寄存器一个堆栈来处理
            (list 'print-stack-statistics
                  (lambda (reg-name)
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
