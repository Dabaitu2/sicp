#lang sicp

(#%require "../../common/data/conventional-interface.rkt")

;; 通过 Horner 规则计算多项式
;; a_nx^n + a_{n-1}x^{n-1}+....+a_1x+a0
;; 可以转换构造为 (...(a_nx+a_{n-1})x+...+a1)x + a0
;; 换句话说，我们可以从an开始，乘以x，再加上an-1，乘以x，如此下去，直到处理完a0
;; 从最外层到最内层依次是 + * + *  ..
;; 所以 acccumulate 也遵循相同的格式
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms))
                )
              0
              coefficient-sequence))

;; 计算 1 + 3x + 5x^3 + x^5 在 x = 2 的值
;; 0 就是系数不存在
(horner-eval 2 (list 1 3 0 5 0 1))
