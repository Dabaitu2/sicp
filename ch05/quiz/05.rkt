#lang sicp

;; 人肉扮演 debugger

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

;; fibnacci hand simulation

;; (fib 4)
;; Stack: null
;; continue: fib-done
;; n: 4
;;
;; after 1st fib-loop
;; Stack: (continue fib-done) (n 3)
;; continue: afterfib-n-1
;; n: 2
;;
;; after 2nd fib-loop
;; Stack: (continue afterfib-n-1) (n 2)
;; continue: afterfib-n-1
;; n: 1
;;
;; after 3rd fib-loop then going to immediate-anwser
;; Stack: (continue afterfib-n-1) (n 2)
;; continue: afterfib-n-1
;; n: 1
;; val: 1
;;
;; jump to afterfib-n-1
;; Stack: (continue afterfib-n-1) (n 3) (val 1)
;; continue: afterfib-n-2
;; n: 0
;;
;; after 4th fib-loop then going to immediate-anwser
;; Stack: (continue afterfib-n-1) (n 3) (val 1)
;; continue: afterfib-n-2
;; n: 0
;; val: 0

;; jump to afterfib-n-2
;; Stack: (continue afterfib-n-1) (n 2) 
;; continue: afterfib-n-1
;; n: 0
;; val: 1
;;
;; jump to afterfib-n-1
;; Stack:  (continue afterfib-n-1) (n 3) (val 1)
;; continue afterfib-n-2
;; n: 0
;; val: 1

;; jump to afterfib-n-2
;; Stack: (continue afterfib-n-1) (n 2) 
;; continue: afterfib-n-1
;; n: 1
;; val: 2
;;
;; jump to afterfib-n-1
;; Stack:  (continue afterfib-n-1) (n 3) (val 2)
;; continue afterfib-n-2
;; n: 1
;; val: 2

;; jump to next fib-loop and going to immediate-anwser
;; Stack:  (continue afterfib-n-1) (n 3) (val 2)
;; continue afterfib-n-2
;; n: 1
;; val: 1


;; ... to complicated, omit ;)
