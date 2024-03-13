#lang sicp


(define (require p)
  (if (not p) (amb)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;; 假设我们想写出一个算法，在理论上可以获取所有毕达哥拉斯三元组(而不是存在上界)
;; 但是我们无法直接使用无穷的 an-integer-starting-from 去替代 an-integer-between
;; 这是因为 require 不满足的情况下，k 会无限增长
;; i 和 j 的增长将根本无法处理到, 永远都是 1^2 + 1^2 == k^2
;; 永远不可能找到解
(define (pythagorean-triples-bad)
  (let ([i (an-integer-starting-from 1)])
    (let ([j (an-integer-starting-from i)])
      (let ([k (an-integer-starting-from j)])
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))



(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))


;; 因此我们需要改变写法，让 j 和 k 要存在上界，而这个上界就是最外层的 i 提供的
(define (pythagorean-triples)
  (let ([i (an-integer-starting-from 1)])
    (let ([j (an-integer-between 1 i)])
      (let ([k (an-integer-between 1 j)])
        (newline)
        (display i)
        (display ",")
        (display j)
        (display ",")
        (display k)
        (require (= (+ (* k k) (* j j)) (* i i)))
        (list k j i)))))

;; (pythagorean-triples-bad)
(pythagorean-triples)
