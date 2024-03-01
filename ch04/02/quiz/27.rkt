#lang sicp

;;; ;;; L-Eval input:
;;; (define count 0)
;;; 
;;; ;;; L-Eval value
;;; #<void>
;;;
;;; ;;; L-Eval input:
;;; (define (id x) (set! count (+ count 1)) x)
;;; ;;; L-Eval value
;;; #<void>
;;; 
;;; ;;; L-Eval input:
;;; 这一步的时候，eval (id (id 10)) 由于求的是一个 application，因此 id 会被调用
;;; 然而，由于复合过程是 non-strict 的，因此内部的 (id 10) 不会在此时被求值, 因此 set 会被调用一次，结果为 count -> 1
;; ;;; (define w (id (id 10)))
;;; 
;;; ;;; L-Eval value
;;; #<void>
;;; 
;;; ;;; L-Eval input:
;;; count
;;; 
;;; ;;; L-Eval value
;;; 1
;;; 
;;; ;;; L-Eval input:
;;; w
;;; 在这里，我们输入 w 实际上就是在获取对应的值，我们在 loop 中最终一定要求值 w 对应的东西
;;; 因此内部的 (id 10) 此时会被求值，因此 set 会被调用一次，结果为 count -> 2
;;; 同时内部的 10 会被返回，w -> 10
;;; 
;;; ;;; L-Eval value
;;; 10
;;; 
;;; ;;; L-Eval input:
;;; count
;;; 这个时候再检查 count 就是 2 了
;;; 
;;; ;;; L-Eval value
;;; 2
