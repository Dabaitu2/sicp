#lang sicp

;; a.
;; 只有 force 的东西不是 thunk 的时候才返回这个东西
(define (unmemorized-force-it obj)
  (if (thunk? obj)
      ;; 还要再检查一层，因为 thunk 可能还会包含 thunk
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; fibbonacci 作为树状结构，就很适合用来检验记忆能力
(define (fib n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

;;; without memorization
;;;;;; L-Eval value
;;;#<void>
;;;
;;;;;; L-Eval input:
;;;(fib 15)
;;;
;;;(Time Taken:  204076)
;;;
;;;;;; L-Eval value
;;;610


;;; with memorization
;;;
;;; L-Eval value
;;;#<void>
;;;
;;;;;; L-Eval input:
;;;(fib 15)
;;;
;;;(Time Taken:  61580)
;;;
;;;;;; L-Eval value
;;;610



;; b.
;; with memo
;; ;;; L-Eval input:
;;; (define count 0)
;;; 
;;; (Time Taken:  26)
;;; 
;;; ;;; L-Eval value
;;; #<void>
;;; 
;;; ;;; L-Eval input:
;;; (define (id x) (set! count (+ 1 count)) x)
;;; 
;;; (Time Taken:  65)
;;; 
;;; ;;; L-Eval value
;;; #<void>
;;; 
;;; ;;; L-Eval input:
;;; (define (square x) (* x x))
;;; 
;;; (Time Taken:  33)
;;; 
;;; ;;; L-Eval value
;;; #<void>
;;; 
;;; ;;; L-Eval input:
;;; (square (id 10))
;;; 
;;; (Time Taken:  47)
;;; 
;;; ;;; L-Eval value
;;; 100
;;; 
;;; ;;; L-Eval input:
;;; count
;;; 
;;; (Time Taken:  14)
;;; 
;;; ;;; L-Eval value
;;; 1


;;; without memo
;;;;;; L-Eval input:
;;; 
;;; (define count 0)
;;; (Time Taken:  26)
;;; 
;;; ;;; L-Eval value
;;; #<void>
;;; 
;;; ;;; L-Eval input:
;;; (define (id x) (set! count (+ 1 count)) x)
;;; 
;;; (Time Taken:  32)
;;; 
;;; ;;; L-Eval value
;;; #<void>
;;; 
;;; ;;; L-Eval input:
;;; (define (square x) (* x x))
;;; 
;;; 
;;; (Time Taken:  32)
;;; 
;;; ;;; L-Eval value
;;; #<void>
;;; 
;;; ;;; L-Eval input:
;;; (square (id 10))
;;; 
;;; (Time Taken:  50)
;;; 
;;; ;;; L-Eval value
;;; 100
;;; 
;;; ;;; L-Eval input:
;;; count
;;; 
;;; (Time Taken:  14)
;;; 
;;; ;;; L-Eval value
;;; 2 ==> things change here
;;; 因为 square 内部 (* x x) 中的 x 都是 (id 10) 这导致
;;; (id 10) 被执行了两遍且没有缓存，所以 set 副作用也执行了两遍
