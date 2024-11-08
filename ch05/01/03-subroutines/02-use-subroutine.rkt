#lang sicp

;; 使用 continue 去避免重复的控制器序列 
gcd
 (test (op =) (reg b) (const 0))
 (branch (label gcd-done))
 (assign t (op rem) (reg a) (reg b))
 (assign a (reg b))
 (assign b (reg t))
 (goto (label gcd))
gcd-done
 (test (op =) (reg continue) (const 0))
 (branch (label after-gcd-1))
 (goto (label after-gcd-2))
  ...
  ;; Before branching to gcd from the first place where it is needed
  ;; we place 0 in the continue register
 (assign continue (const 0))
 (goto (label gcd))
after-gcd-1
 ...
 ;; Before the second use of gcd, we place 1 in the continue register
 (assign continue (const 1))
 (goto (label gcd))
after-gcd-2
