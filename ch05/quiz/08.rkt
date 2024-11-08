#lang sicp

(controller 
  start
    (goto (label here))
  here
    (assign a (const 3))
    (goto (label there))
  here
    (assign a (const 4))
    (goto (label there))
  there)

;; 对于已有的模拟器，当控制到达 there 时, a 的值为 3
;; 在进行 controller text 的扫描和 assemble 时,
;; 我们的 receive 是由上到下执行的， 也就是对第一个 label here 的处理会在对第二个 label here 的处理"里面"
;; 进行， 也就是先会去对第二个 label here 处理， 然后处理第一个， 又因为我们插入 label / instruction 的方式
;; 是 (cons new rest), 因此后插入的会在前面， 对应的处理过程自然也就是第一个 here 对应的， 也就是会把 a 搞成 3

(define (extract-labels text receive)
  (if (null? text)
      ;; 如果 text 为空，经由外部的 update-insts! 将 machine 更新为空，并返回空的 instruction list
      ;; 这个时候才会触发第一次 receive (实际上已经被包装过 n 层)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ([next-inst (car text)])
           (if (symbol? next-inst)
               ;; 增加对 labels 的重名检查
               (let ([s (assoc next-inst labels)])
                 (if s
                     (error "Duplicate labels -- ASSEMBLE"
                            s)
                     (receive
                      insts
                      (cons (make-label-entry next-inst
                                              insts)
                            labels))))

               ;; 否则认为是指令的一部分
               ;; 就往指令序列中插入一个指令
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))
