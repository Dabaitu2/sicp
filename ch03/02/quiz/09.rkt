#lang racket

;; 递归阶乘
(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))

;; 迭代阶乘
(define (factorial-iter n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(factorial 6)

;; 对递归：
;; global-env: factorial -> [body|n, global-env]
;; (factorial 6) 
;; -> new Env(n->6, (if (= n 1) 1 (...))) => (if #f 1 (* n (factorial (- n 1))))
;; (* n (factorial (- 6 1))) -> (* n (factorial 5)) 
;; -> new Env(n->5, if (= n 1) 1 (...))) => if (#f 1 (* n factorial (- n 1)))
;; (* n (factorial (- 5 1))) -> (* n (factorial 4))
;; -> ...


;; 对迭代：
;; global-env: {factorial-iter -> [body|n, global-env], fact-iter -> [product counter max-count]}
;; (factorial-iter 6)
;; -> new Env(n->6, (fact-iter 1 1 6))
;; -> new Env(product->1, counter->1 max-count->6, (if (> ...) 1 (fact-iter ...))) -> (if #f 1 (fact-iter ...))
;; -> new Env(product->1, counter->2, max-count->6, (if (> ...) 1 (fact-iter ...))) -> if (#f 1 (fact-iter ...))
;; ,..
;; -> new Env(product-> 720, counter->7, max-count->6, ... if (#t 720 ...)))
;; -> 720 then trace back
;; 可以发现这里的环境模型目前还没有办法解释尾递归，因为被最后的 if 执行出得结果还得一层层的回溯回去
;; 第五章会有进一步的解释
