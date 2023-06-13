#lang racket


;; 为了解决 2.95 中遇到的非整数计算的问题，我们可以利用 "伪除" 来实现目标
;; 朴素的说，便是在 gcd 中执行多项式除法之前为被除式乘上一个常数因子
;; 在最后 gcd 的时候，我们找到最后的存在放大系数的 term-list, 求出其系数的最大公因数再从每个 term 的系数中除去即可


;; 而这个常数因子可以如下取得：
;; 如果 P, Q 均为多项式, 令 O1 是 P 的最高项次数, O2 是 Q 的最高次数
;; c 是 Q 的首项系数
;; 那么我们可以设这样一个整数化因子 C^1+O1-O2 满足我们的要求

;; a) 实现 pseudoremainder-terms
(define (pseudoremainder-terms a b)
  (let ([o1 (order (first-term a))]
        [o2 (order (first-term b))]
        [c (coeff (first-term b))])
    (let ([factor (expt c (add 1 (sub o1 o2)))])
      (cadr (div-terms (mul-term-by-all-terms
                        (make-term 0 factor)
                        a)
                       b)))))

(define (div-coeff-list-gcd termlist)
  (let ((coeff-list (map coeff termlist)))
    (let ((gcd-coeff (fold-left gcd (car coeff-list) coeff-list)))
      (contents (car (div (tag termlist) gcd-coeff)))
      ))
  )

(define (gcd-terms a b)
  (if (empty-termlist? b)
      (div-coeff-list-gcd a)
      (gcd-terms b (pseudoremainder-terms a b))))
