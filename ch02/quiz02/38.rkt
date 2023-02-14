#lang sicp

;; fold-right 其实就是 accumulate
(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op (car rest) result)
              (cdr rest))))
  (iter initial sequence))

;; 区别只在于参数位置
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


;; 3 / 1 => 3  2 / 3 => 2 / 3  1 / 2 / 3 => 3 / 2
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3)) ;; => (1 (2 (3 nil)))
(fold-left list nil (list 1 2 3))  ;; =>(((nil 1) 2) 3)

(#%provide fold-left fold-right)
