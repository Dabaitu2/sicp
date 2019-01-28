#lang sicp
(define
  (square x)
  (* x x))
;; 迭代计算过程计算阶乘
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
;; 计算帕斯卡三角形
;; ps: 帕斯卡三角形的每一行恰好是二次项系数
(define (pascal-triangle x y)
  (cond ((> y x)
         (error "invalid code"))
        ((or (= y 0) (= y x)) 1)
        (else (+ (pascal-triangle (- x 1) (- y 1) )
                 (pascal-triangle (- x 1) y)))))
(pascal-triangle 4 2)
;; 迭代版本的pascal三角形
;; 下标从0开始, 需要借助公式 p(row, col) = row!/col!(row-col)!
(define (pascal row col)
  (/ (factorial row)
     (* (factorial col)
        (factorial (- row col)))))
(pascal 4 2)
;; 递归计算求幂
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
;; 迭代计算求幂
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* product b))))
;; 如果是偶次幂，可以快速求解b^4 = (b^(4/2))^2，如果是奇数，可以进行一轮正常求解后转化为偶次幂求解
;; 递归算法
(define (even? n)
  (= (remainder n 2) 0))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))