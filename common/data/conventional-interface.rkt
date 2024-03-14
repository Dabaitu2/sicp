#lang sicp

(define nil '())

(define (deep-reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (let ([first (car items)])
                (cons (if (pair? first)
                          (iter first nil)
                          (car items))
                      result)))))
  (iter items nil))

(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
        result
        (reverse-iter (cdr items)
                      (cons (car items) result))))
  (reverse-iter items nil))

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

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; 实现 accumulate (reduce / fold)
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

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

;; 我们通常可以把这样的操作抽象为 flatmap
;; 它用来 append 累积映射 map 结果
;; (由于映射结果是 list, append 这些 list 会消掉这些 list 的括号, 就像是将嵌套 list 打平了, 故命名 flatmap)
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (distinct? items)
  (cond
    [(null? items) true]
    [(null? (cdr items)) true]
    [(member (car items) (cdr items)) false]
    [else (distinct? (cdr items))]))

(#%provide map
           flatmap
           filter
           fold-left
           fold-right
           for-each
           accumulate
           accumulate-n
           enumerate-interval
           reverse
           deep-reverse
           distinct?)
