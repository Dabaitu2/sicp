#lang sicp

;; 这个 test 需要在我们实现了具体代码之后才能真实启动
(define recursive-expt-machine
  (make-machine
    '(product continue n val b)
    (list (list '= =) (list '- -) ('* *))
    '(expt-loop
        (test (op =) (reg n) (const 0))
        (branch (label base-case))
        ;; 习惯性动作， 因为马上要覆盖 continue ,在这之前保存一下上下文
        ;; 这里 n 不用记忆，因为我们后面用不到历史的 n 了
        ;; 执行完 loop 后会被 after-expt 恢复
        (save continue)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-expt))
        (goto (label expt-loop))
      after-expt
        (restore continue)
        (assign val (op *) (reg b) (reg val))
        (goto (reg continue))
      base-case
        (assign (reg val) (const 1))
        (goto (reg continue))
      expt-done)))

(set-register-contents! recursive-expt-machine 'n 4)
;; done
(set-register-contents! recursive-expt-machine 'b 2)
;; done
(get-resgiter-contents recursive-expt-machine 'val)
;; value: 16


(define iter-expt-machine
  (make-machine
    '(product counter b)
    (list (list '= =) (list '- -) (list '* *))
    '((assign product (const 1))
        expt-loop
         (test (op =) (reg counter) (const 0))
         (branch (label expt-done))
         (assign counter (op -) (reg counter) (const 1))
         (assign product (op *) (reg b) (reg product))
         (goto expt-loop)
        expt-done
      )))

(set-register-contents! iter-expt-machine 'b 2)
;; done
(set-register-contents! iter-expt-machine 'counter 4)
;; done
(get-resgiter-contents iter-expt-machine 'product)
;; 16
