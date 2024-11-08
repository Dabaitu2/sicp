#lang sicp

;; (#%require "./combine.rkt")
;; (#%require "./combine-03.rkt")
;; (#%require "./combine-quiz12-13.rkt")
(#%require "./combine-04.rkt")

;; 一个 gcd machine 的实例
;; (define gcd-machine
;;   ;; (make-machine '(a b t)
;;   (make-machine
;;    (list (list 'rem remainder) (list '= =))
;;    '(test-b (test (op =) (reg b) (const 0))
;;             (branch (label gcd-done))
;;             (assign t (op rem) (reg a) (reg b))
;;             (assign a (reg b))
;;             (perform (op print-stack-statistics) (const t))
;;             (assign b (reg t))
;;             (goto (label test-b))
;;             gcd-done)))
;;
;; (set-register-contents! gcd-machine 'a 206)
;; ;; done
;;
;; (set-register-contents! gcd-machine 'b 40)
;; ;; done
;;
;; (start gcd-machine)
;; ;; done
;;
;; (get-register-contents gcd-machine 'a)

;; 一个非递归的 factorial machine 的实例
(define fact-machine
  (make-machine
   (list (list '= =) (list '* *) (list '- -))
   '((assign continue (label fact-done))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     fact-done)))

(set-register-contents! fact-machine 'n 8)
(start fact-machine)
(get-register-contents fact-machine 'val) ;; 40320

;; (gcd-machine 'get-insts-groups)
;; (gcd-machine 'get-entry-points)
;; (gcd-machine 'get-stack-regs)
;; (gcd-machine 'get-sources)
