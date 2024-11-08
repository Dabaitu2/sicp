#lang sicp

;; 设计一种新的寄存器语法， 在不影响其他部分的情况下将其
;; 添加到我们的模拟器实现中

;; 设计一个累加 inc 指令，为寄存器的当前值 + 1
;; 简单点， 暂时不支持 label
;; (inc <reg-name>)
(define (make-inc inst machine labels pc)
  (let ([target (get-register machine
                              (inc-reg-source inst))])
    (lambda ()
      (set-contents! target (+ 1 (get-contents target)))
      (advance-pc pc))))

(define (inc-reg-source inst)
  (cadr inst))
