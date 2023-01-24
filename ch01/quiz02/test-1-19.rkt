#lang racket

;; 快速方法求斐波那契数
;; 注意到fibonacci数列满足以下递推式子T: a<-b+a b<-a
;; 该式子可以看作 p=0,q=1 情况下的递推式T_pq a<-bq+aq+ap b<-aq+bp
;; 
;; 我们现在证明，T_pq 对 pair(a, b) 进行两次变换所得到的效果等同于存在的一个 T_pq', 其 p' q' 可以通过 pq 得出
;;
;; 将Tpq进行两次变换(称作T的2次方),
;; 第一次 是 a <- (p+q)a + bq, b <- aq + bp
;; 第二次 是 a <- (p+q)((p+q)a + bq) + (aq+bp)q,  b <- ((p+q)a + bq)q + (aq+bp)p
;;
;; 可以得到Tp'q' a <- (2pq+q^2)b + (p^2+q^2+2pq+q^2)a -> a(p^2 + q^2) + a(2pq + q^2) + b(2pq + q^2)
;;               b <- (p^2+q^2)b + (2pq+q^2)a
;; 
;; 通过对应就可以发现系数可以代换
;; 从而得出p' = p^2 + q^2 
;;         q' = 2pq + q^2 
;;
;; 也就是说对每两次递推T 可以通过变换 p, q 快速取得

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (square n)
    (* n n))
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ;; p' = p^2 + q^2
                   (+ (* 2 p q) (square q))  ;; q' = 2pq + q^2
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b q) (* a q))
                        p
                        q
                        (- count 1)))))
(fib 110)
