#lang sicp

;; 验证无穷连分式是否收敛于黄金分割率
(define (cont-frac n d k)
  (define (iter now res)
    (if (= now 0)
        res
        (iter (- now 1) (/ (n now) (+ (d now) res)))))
  (iter k 0))

(define (cont-frac-2 n d k)
  (define (cf i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i) (+ (d i) (cf (+ i 1)))))
    )
  (cf 1)
  )

(cont-frac-2 (lambda (i) 1.0) (lambda (i) 1.0) 1000)

(#%provide cont-frac)
