#lang sicp


;; gcd with read & print
;; 这长得很像第四章我们见过的 driver-loop

(controller
 gcd-loop
   (assign a (op read))  ;; read 就像一个常规操作可以产出值， 但是值是来自于用户输入
   (assign b (op read))
 test-b
   (test (op =)
         (reg b)
         (const 0))
   (branch (label gcd-done))
   (assign t
           (op rem)
           (reg a)
           (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done
   (perform (op print)  ;; 为了打印， 我们需要使用 perform 去链接一个输入和一个 operation: print
            (reg a))
   (goto (label gcd-loop)))
