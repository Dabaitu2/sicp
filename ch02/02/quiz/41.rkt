#lang sicp

(#%require "../../common/conventional-interface.rkt")
(#%require "../../common/math/num-theory.rkt")

;; helpers
(define (equal-sum? pair sum)
  (= (+ (car pair) (cadr pair)) sum))

;; 生成 (i, j, i+j)
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; 抽象中间过程
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


(define (prime-sum-pairs n sum)
  (map make-pair-sum
       (filter (lambda (pair) (equal-sum? pair sum))
               (unique-pairs n))))

(unique-pairs 6)
(prime-sum-pairs 6 10)
