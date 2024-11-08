#lang sicp

;; a. 这是一个类似 factorial 的过程， 不是尾递归
(define (expt b n)
  (if (= n 0) 1 (* b (expt b (- n 1)))))


;; 与之对应的， 其 controller description 如下:
(contoller
 (assign continue (label expt-done))
 expt-loop
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
 expt-done)


;; b. 这是一个迭代过程, 也就是尾递归
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
      product
      (expt-iter (- counter 1)
                 (* b product))))
  (expt-iter n 1))


;; 与之对应的 controller description 如下:
(controller
 (assign product (const 1))
 expt-loop
  (test (op =) (reg counter) (const 0))
  (branch (label expt-done))
  (assign counter (op -) (reg counter) (const 1))
  (assign product (op *) (reg b) (reg product))
  (goto expt-loop)
 expt-done)
