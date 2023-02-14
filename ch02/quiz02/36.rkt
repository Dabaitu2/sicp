#lang sicp

(#%require "../../common/conventional-interface.rkt")

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      ;; 取出 seqs 的第一项 map 为新的 list 做 accumulate 得到结果后，递归计算剩下的
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
            (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

;; -> (1+4+7+10, 2+5+8+11, 3+6+9+12)
;; -> (22, 26, 30)
(accumulate-n + 0 (list (list 1 2 3)
                        (list 4 5 6)
                        (list 7 8 9)
                        (list 10 11 12)))
