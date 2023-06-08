#lang racket

;; 只支持数值的 gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;; 实际上我们还可以支持对 term-list 做 gcd, 算法如下
;; 本题就是实现这里的剩余需要的子方法
(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ([t1 (first-term L1)] [t2 (first-term L2)])
        (if (> (order t2) (order t1))
            ;; if order does not meet the condition
            ;; we will take the dividend as the final remainder
            (list (the-empty-termlist) L1)
            ;; c coeff
            ;; o order
            (let ([new-c (div (coeff t1) (coeff t2))]
                  [new-o (- (order t1) (order t2))])
              (let ([rest-of-result
                     ;; 递归计算部分
                     (div-terms
                      (sub-terms
                       L1
                       (mul-term-by-all-terms
                        (make-term new-o new-c)
                        L2))
                      L2)])
                ;; 组合形成完整结果
                (list (adjoin-term (make-term new-o new-c)
                                   (car rest-of-result))
                      (cadr rest-of-result))))))))

;; remainder-terms 其实就是使用 div-terms 获得的结果的 余式部分
(define (remainder-terms a b)
  (cadr (div-terms a b)))
