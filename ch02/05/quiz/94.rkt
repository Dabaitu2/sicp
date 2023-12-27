;; GCD 在代码中的运用通常都是 欧几里得算法(辗转相除法)，即 Euclid's Algorithm
;; 我们在 2.94 中发现欧几里得算法居然对 多项式 polynomial 也适用
;; 这是因为多项式也可以满足一种名为 "Euclidean ring" (欧几里得环) 的代数域
;;
;; 欧几里得环除了在这个代数域中可以进行 加法，减法和可交换的乘法以外
;;
;; 它还基于这样一个形式化的描述
;; 1. 具有 0 值
;; 2. 设 m(x) 为属于此环 D 中的元素的 "测度" 
;; (measure: 用于度量此元素的量级大小, 
;;    对于整数环，这个测度就是它的绝对值
;;    对于多项式环, 这个测度就是多项式的次数 degree
;;    其实这种表述还挺朴素的, 多项式的数值大小确实跟其 degree 相关, 而整数的绝对值自然也代表了这个数的*数值*大小
;;    可以理解为数轴上离原点的距离
;;    )
;; i: 对于任何的非 0 的 x,y ∈ D, m(xy) > m(x)  -> 乘法结果的测度高于单个元素测度
;; ii: 对于任何给定的 x, y ∈ D, 一定存在一个 q 使得 y = qx + r, r满足可能是 0，或者 m(r) < m(x)
;; 这两段描述其实就是对符合使用辗转相除法进行计算的域的形式化描述, 不过这里我们不是要证明他，只是为了说明为什么多项式可以做这种操作

#lang sicp

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
