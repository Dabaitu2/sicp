#lang sicp

(define (require p)
  (if (not p) (amb)))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ([i (an-integer-between low high)]
        [hsq (* high high)])
    (let ([j (an-integer-between i high)])
      (let ([ksq (+ (* i i) (* j j))])
        (require (>= hsq ksq))
        (let ([k (sqrt ksq)])
          (require (integer? k))
          (list i j k))))))
(a-pythagorean-triple-between 1 5)


;; 在忽略 sqrt 在 low high 距离不远的情况下可能出现的资源占用的场景
;; Ben 的方案的确会更有效率，因为这减少了一轮对于 k 的值的产出
;; 原先可能最差情况下会产生 (high - low)^3 轮迭代
;; 而现在迭代被收缩到了 (high - low)^2 轮






