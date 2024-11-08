#lang sicp

;; please see arrows below

(controller
  (assign continue (label fib-done))
 fib-loop
  (test (op <) (reg n) (const 2))
  (branch (label immediate-anwser))
  ;; setup to compute Fib(n-1)
  (save continue)
  (assign continue (label afterfib-n-1))
  (save n)                                    ; save old value of n
  (assign n (op -) (reg n) (const 1))         ; clobber n to n-1
  (goto (label fib-loop))                     ; perform recursive call
 afterfib-n-1                                 ; upon return, val contains Fib(n-1)
  (restore n)                                 
  ;; (restore continue)

  ==> ;; no need, setup to compute Fib(n-2)

  (assign n (op -) (reg n) (const 2))

  ==>;; no need, (save continue)

  (assign continue (label afterfib-n-2))
  (save val)                                  ; save Fib(n-1)
  (goto (label fib-loop))                        
 afterfib-n-2                                 ; upon return, val contains Fib(n-2)
  (assign n (reg val))                        ; n now contains Fib(n-2)
  (restore val)                               ; val now contains Fib(n-1)
  (restore continue)
  (assign val (op +) (reg val) (reg n))       ; Fib(n-1) + Fib(n-2), fib(n-2) 其实也是 fib(n-1) 的子问题
  (goto (reg continue))
 immediate-anwser
  (assign val (reg n))                        ; base case: Fib(n) = n
  (goto (reg continue))
 fib-done)
