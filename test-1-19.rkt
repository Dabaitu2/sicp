#lang sicp
;; 快速方法求斐波那契数
;; 注意到fibonacci数列满足以下递推式子T: a<-b+a b<-a
;; 该式子可以看作p=0,q=1情况下的递推式Tpq a<-bq+aq+p b<-aq+bp
;; 将Tpq进行两次变换(称作T的2次方), 可以得到Tp'q' a<-(2pq+q^2)b+(p^2+q^2 + 2pq+q^2)a b<-(p^2+q^2)b + (2pq+q^2)a
;; 从而得出p' = p^2 + q^2 q = q^2 + 2pq, 也就是说对每两次递推T可以通过变换p, q快速取得
(define (even? n)
  (= (remainder n 2) 0))
(define (square n)
  (* n n))
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b q) (* a q))
                        p
                        q
                        (- count 1)))))
(fib 110)