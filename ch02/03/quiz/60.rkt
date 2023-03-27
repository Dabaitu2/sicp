#lang racket

;; 在集合允许重复的情况下重新设计操作

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

;; 比较麻烦的情况是 求交集，我们需要预先除掉当前值对应的集合对应位置的那个值
(define (remove-set-element x set)
  (define (remove-set-element-iter acc rest)
    (cond ((null? rest) acc)
          ((equal? x (car rest)) (append acc (cdr rest)))
          ;; 如果当前这个不是，就更新 acc 和 rest
          (else (remove-set-element-iter (adjoin-set (car rest) acc) (cdr rest)))))
  (remove-set-element-iter '() set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 (remove-set-element (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

(intersection-set (list 2 3 2 2 1 4 5) (list 1 2 3 3))

;; 这种方法对如果入参已经保证不一致的情况比较友好
;; 其他情况下的 set 会有很多杂质
