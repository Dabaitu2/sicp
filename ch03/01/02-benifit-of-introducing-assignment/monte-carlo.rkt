#lang racket

(define random-init 987654321)
(define a 1103515245)
(define c 12345)
(define m (expt 2 31))

;; 线性同余法求随机数, 这里的几个参数都是随便求的，本身应该有更好的选择
(define (rand-update x)
  (remainder (+ (* a x) c) m))

;; 为了保证我们的随机数具有统计意义上的随机性质, 要看它产出的值是否整体而言符合均匀分布
;; 结合线性同余的数学性质，就能够以最简单的方式保证这一点，不过线性同余每次都要接受一个种子 seed
;; 产生下一个值，并且下一个值需要传给自身再产生下一个 “随机数", 而将每一轮产生的值都存到内部过程
;; 就可以避免反复的 "生成" "传入" 逻辑了
(define rand
  (let ([x random-init])
    (lambda ()
      (set! x (rand-update x))
      x)))

;; 蒙特卡罗方法：
;; 从一个大集合中随机选取样本, 并对这些试验结果的统计估计的基础上做出推断
;; 例如， 已知 6/π^2 是随机选取两个整数之间没有公共因子的概率 (cesaro 定理)
;; 我们可以通过选取足够多的样本，通过蒙特卡罗方法获得这个 6/π^2 的近似数值
;; 进而推算出 π 的近似值

;; 试验次数 实验过程
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      [(= trials-remaining 0) (/ trials-passed trials)]
      [(experiment)
       (iter (- trials-remaining 1) (+ trials-passed 1))]
      [else (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))

;; 通过将 rand 封装为无参数过程并使用内部暂存变量，我们反复调用它可以保证其数据始终不同
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

;; the result is not we expected, that's because our LCG is too naive ..
;; (estimate-pi 100000)


;; mento carlo integration
(define (random-in-range low high)
  (let ([range (- high low)]) (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (is-in-integral-function?)
    (let ([x (random-in-range x1 x2)]
          [y (random-in-range y1 y2)])
      (P x y)))
  (let ([base-area (* (- x2 x1) (- y2 y1))])
    (* base-area
       (monte-carlo trials is-in-integral-function?))))
