#lang racket

;; 实现一个过程 f, 使得下面的表达式采用从左到右和从右到左求解子表达式的结果不同
;; 对实际参数采 用从左到右的求值顺序时返回0，而对实际参数采用从右到左的求值顺序时返回1
(define f
  (let ([init 0] [modified 0])
    (lambda (num)
      (if (= modified 0)
          (begin
            (set! init num)
            (set! modified 1)
            num)
          init))))

(+ (f 0) (f 1))
