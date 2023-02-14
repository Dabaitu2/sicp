#lang sicp

;; lisp 中的 “扩展运算符”
;; dotted-tail notation

;; 传入一系列参数，保留所有与第一项相同奇偶性的数据
(define (same-parity first . rest)
  (define (same-parity-iter items result remainder-val)
    (if (null? items)
      result
      (same-parity-iter 
        (cdr items)
        (if (= (remainder (car items) 2) remainder-val)
               (append result (list (car items)))
               result)
        remainder-val))
    )
  (same-parity-iter rest (list first) (remainder first 2)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
