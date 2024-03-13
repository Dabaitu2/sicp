#lang sicp

(define (require p)
  (if (not p) (amb)))

;; 寻找毕达哥拉斯三元组
(define (a-pythagorean-triple-between low high)
  (let ([i (an-integer-between low high)])
    (let ([j (an-integer-between i high)])
      (let ([k (an-integer-between j high)])
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))


(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

;; 不可以使用这种策略，因为 an-integer-starting-from 永远不会终止
;; 更详细的说，无穷的 amb 的嵌套要小心，因为内部的 amb 会使得内部的 amb 重复尝试
;; 我们最好把无穷 amb 的调用权交给外部, 除非我们确保 amb 调用对应的过程会终止
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between-bad low high)
  (require (<= low high))
  (let ([test (an-integer-starting-from low)])
    (require (>= high test))
    test))


(an-integer-between 3 5)
(amb)
(amb)

(a-pythagorean-triple-between 3 11)
