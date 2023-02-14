#lang racket

(define nil '())

;; 实现 map
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))

;; 实现 filter
(define (filter predicate sequence)
  (cond [(null? sequence) nil]
        [(predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]
        ))

;; 实现 for-each
(define (for-each proc items)
  (let ([items-cdr (cdr items)])
    (proc (car items))
    (if (not (null? items-cdr))
        (for-each proc items-cdr)
        #t)))

;; 实现 accumulate (reduce / fold)
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


;; 针对 list 每一个 list 的相同元素做累加操作形成新的 list
;; (accumulate-n + 0 ((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
;; -> (1+4+7+10, 2+5+8+11, 3+6+9+12)
;; -> (22, 26, 30)
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      ;; 取出 seqs 的第一项 map 为新的 list 做 accumulate 得到结果后，递归计算剩下的
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
            (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(#%provide map
           flatmap
           filter
           for-each
           accumulate
           accumulate-n
           enumerate-interval)
