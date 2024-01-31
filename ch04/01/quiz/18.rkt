#lang sicp

;; another way to scan the internal definition

;; (lambda <vars>
;;   (let ((u '*unassigned*)
;;         (v '*unassigned*))
;;     (let ((a <e1>)
;;           (b <e2>))
;;       (set! u a)
;;       (set! v b))
;;     <e3>))


;; 考虑 3.5.4 的 solve
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; 练习中给出的 scan 方式无法正确解析此方法
;; 因为在解析 b 时，要求 y 已经被求值了, 而此时 y 还是不存在的 
;; 我们必须保证 e2 被求值前 e2 中可能涉及到的 u 已经被求值了
;; 而在正文中给出的方案, 求值 e2  前 e1 肯定被求值了，u 也被求值了，所以能够正常执行
