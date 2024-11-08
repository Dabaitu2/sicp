#lang sicp

;; Use cases:

;; 构造一台寄存器机器模型
(make-machine <register-names> <operations> <controller>)

;; 将一个值存入给丁机器的一个被模拟的寄存器
(set-register-contents! <machine-model>
                        <register-name>
                        <value>)

;; 返回给定机器里一个被模拟的寄存器的内容
(get-resgiter-contents <machine-model> <register-name>)

;; 启动机器模型
(start <machine-model>)

;; 一个 gcd machine 的实例
(define gcd-machine
  (make-machine '(a b t)
                (list (list 'rem remainder) (list '= =))
                '(test-b 
                   (test (op =) (reg b) (const 0))
                   (branch (label gcd-done))
                   (assign t (op rem) (reg a) (reg b))
                   (assign a (reg b))
                   (assign b (reg t))
                   (goto (label test-b))
                 gcd-done)))

(set-register-contents! gcd-machine 'a 206)
;; done

(set-register-contents! gcd-machine 'b 40)
;; done

(start gcd-machine)
;; done

(get-resgiter-contents gcd-machine 'a)
