#lang sicp

(define (repeated f base)
  ;; 函数的重复运用
  (define (iter counter result)
    (if (= 1 counter)
        result
        (iter (- counter 1) (compose f result)))
    )
  (iter base f))

;; n次平均阻尼函数
(define (multi-average-damp f n)
  (repeated (average-damp f) n))

;; 返回一个过程，这个过程接受一个参数，计算这个参数的n次方，并对结果做damp-time次平均阻尼处理
(define (damp-nth-root n damp-time)
  (lambda (x)
    (fixed-point
     (multi-average-damp
      (lambda (y)
        (/ x (expt y (- n 1))))
      damp-time)
     1.0)))

;; 实验证明需要的damp次数为[lgn](对logn取整)
(define (lg n)
  (cond ((> (/ n 2) 1) (+ 1 (lg (/ n 2)))) ;; 这一步的理由是， n>2 说明 n/2 > 1，logn = log(n/2) + log(2) log(2) = 1 故可以这样递归的求解log(n/2)
        ((< (/ n 2) 1) 0)
        (else 1)))

(define (nth-root n)
  (damp-nth-root n (lg n)))

(define sqrt (nth-root 3))
(sqrt 4)
