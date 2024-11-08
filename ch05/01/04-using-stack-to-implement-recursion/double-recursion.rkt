#lang sicp

(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))


;; controller for describing the above fib procedure
;; 其实 fib-loop, afterfib-n-1, afterfib-n-2, immediate-answer 都是 subroutine
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
  ;; no need, setup to compute Fib(n-2)
  (assign n (op -) (reg n) (const 2))
  ;; no need, (save continue)
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


;; subroutine:
;;
;; fib-loop n
;;   if base case 
;;      then goto immediate-anwser
;;   else
;;      we will try to compite fib(n-1), so:
;;
;;      set n = n - 1
;;      set continue = afterfib-n-1
;;      goto fib-loop to compute fib(n-1)
;;
;;
;; immediate-anwser n
;;   set val = n
;;   goto continue
;;     if fib-done, then just finish
;;     if afterfib-n-1 / afterfib-n-2, then execute it
;;
;;
;; afterfib-n-1
;;   we will try to compute fib(n-2)
;;   to compute fib(n-2), we need get the original n (now has been set to n-1), so:
;;  
;;   restore n from stack
;;   set n = n - 2
;;   set continue = afterfib-n-2
;;   save val(computed by fib(n-1)) to stack, laterly it will be used as part of adding (+ (fib (- n 1)) (fib (- n 2)))
;;   goto fib-loop to compute fib(n-2) 
;;
;;
;; afterfib-n-2
;;   we will try to get result of fib(n-1) + fib(n-2)
;;
;;   set n = val
;;   restore val from stack to get pre-calculated fib(n-1)
;;   restore continue stack to get the original continue
;;   set val = val + n
;;   goto continue to return to outter level stack
;;
;;
;; ;; 实际上本身我们的寄存器机器也是类似 DFS 策略先运行到最底层, 再逐渐向上返回的
;;
;;   fib 3
;; 1  |
;;   fib 2 + fib 1
;;    |     6 |  | 7 
;; 2  |-------+  +--------> omit others
;;    |
;;   fib 1 + fib 0
;; 3  |     4 | | 5
;;    1-------  0
