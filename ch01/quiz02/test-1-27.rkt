#lang sicp

;; 检测 Carmichael 数字是否的确可以骗过费马检查
(define (square n) (* n n))

(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp) (remainder (square (expmod base (/ exp 2) m)) m)]
        [else (remainder (* base (expmod base (- exp 1) m)) m)]
        ))

(define (Carmichael-test n)
  ;; 对于每一个 a ，检查 a^n 和 a 是否 模 n 同余
  (define (try-it a)
    (= (expmod a n n) a))
  ;; 检测 < n 的所有 a
  (define (test-iter a)
    (cond ((= a 1) #t)
          ((try-it a) (test-iter (- a 1)))
          (else #f)))
  (test-iter (- n 1)))

(Carmichael-test 1105)
