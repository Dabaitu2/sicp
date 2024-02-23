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
;;; (define w (id (id 10)))
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
;;; 
;;; ;;; L-Eval value
;;; 10
;;; 
;;; ;;; L-Eval input:
;;; count
;;; 
;;; ;;; L-Eval value
;;; 2
