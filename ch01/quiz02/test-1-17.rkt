#lang sicp

;; 乘法可以看作反复做加法求出
;; a * b =>  a + a +... + a (共 b 次)
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
(* 3 4)

;; 用类似求快速幂的方法求乘积(递归计算方法)
;; 4 * 8 = 8 * 4 = 16 * 2 = 32 * 1 = 32
(define (multi a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond ((= 1 b) a)
        ((even? b) (multi (double a) (halve b)))
        (else (+ a (multi a (- b 1))))))

(multi 9 9)
