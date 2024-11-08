#lang sicp

;; simple controller for tail-recursive gcd
(controller
 test-b                                     ; label
   (test (op =) (reg b) (const 0))          ; test 指令, 并同时指定操作符和输入
   (branch (label gcd-done))                ; 分支指令, 若上面的 test 结果为真, 则跳到 gcd-done, 否则下一步
   (assign t (op rem) (reg a) (reg b))      ; 赋值指令, 将 register a 和 b 的值送给 operation rem 的输入，将产出的结果送给 register t
   (assign a (reg b))                       ; 将 register b 的值赋给 register a
   (assign b (reg t))                       ; 将 register t 的值赋给 register b
   (goto (label test-b))                    ; 无条件跳转回开头
 gcd-done)


;; complex controller for non-tail-recursive factorial
(controller
  (assign continue (label fact-done)) ;set up final return address
fact-loop
  (test (op =) (reg n) (const 1))
  (branch (label base-case))
  ;; Set up for the recursive call by saving n and continue.
  ;; Set up continue so that the computation will continue
  ;; at after-fact when the subroutine returns.
  (save continue)
  (save n)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-fact))
  (goto (label fact-loop))
after-fact
  (restore n)
  (restore continue)
  (assign val (op *) (reg n) (reg val))  ;val now contains n(n - 1)!
  (goto (reg continue))                  ;return to caller
base-case                                
  (assign val (const 1))                 ;base case: 1! = 1
  (goto (reg continue))                  ;return to caller
fact-done)
