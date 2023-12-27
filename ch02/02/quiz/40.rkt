#lang sicp

(#%require "../../../common/data/conventional-interface.rkt")
(#%require "../../../common/math/num-theory.rkt")

;; helpers
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;; 生成 (i, j, i+j)
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; 抽象中间过程
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


(unique-pairs 6)
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(prime-sum-pairs 6)
