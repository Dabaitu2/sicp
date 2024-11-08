#lang sicp

;; 数据通路 + controller 描述
;; 定义了一些寄存器和操作
(data-paths
;; 定义寄存器
 (registers
   ;; 一个寄存器 = name + button
  ((name a)
   ;; 一个 button = name + source
   ;; 一个 source = 一个数据源 (register / 常量 / operation)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem))))))
 ;; 定义操作
 ;; 一个 operation = name + 输入 list
 ;; 一个输入 = 一个或多个数据源 register / 常量
 (operations
  ((name rem) (inputs (register a) (register b)))
  ((name =) (inputs (register b) (constant 0)))))


;; 定义控制器流水线 (指令序列)
;; 一个控制器也有一个名字 (label)
(controller
 test-b                      ; label
   (test =)                  ; test 指令, 测试 = 操作的情况
   (branch (label gcd-done)) ; conditional branch 分支指令，若结果为真，则跳到 gcd-done, 否则下一步
   (t<-r)                    ; button push
   (a<-b)                    ; button push
   (b<-t)                    ; button push
   (goto   (label test-b))   ; unconditional branch  ;; 无条件跳转回开头
 gcd-done)                   ; label



;; 仅保留 controller 描述, 混合数据通路指令
;; 其实感觉这玩意可读性更差了..
(controller
 test-b                                     ; label
   (test (op =) (reg b) (const 0))          ; test 指令, 并同时指定操作符和输入
   (branch (label gcd-done))                ; 分支指令, 若上面的 test 结果为真, 则跳到 gcd-done, 否则下一步
   (assign t (op rem) (reg a) (reg b))      ; 赋值指令, 将 register a 和 b 的值送给 operation rem 的输入，将产出的结果送给 register t
   (assign a (reg b))                       ; 将 register b 的值赋给 register a
   (assign b (reg t))                       ; 将 register t 的值赋给 register b
   (goto (label test-b))                    ; 无条件跳转回开头
 gcd-done)
