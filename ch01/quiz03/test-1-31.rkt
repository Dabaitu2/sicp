#lang sicp

;; product 高阶抽象
;; 递归版本
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; 迭代版本
(define (product2 term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (pi-term n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))

(define (next x) (+ x 1))

(* (product pi-term 1.0 next 6) 4)   ;;= 3.3436734693877552
(* (product pi-term 1.0 next 100) 4) ;;= 3.1570301764551676

