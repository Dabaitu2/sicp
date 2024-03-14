#lang sicp


(define (require p)
  (if (not p) (amb)))

;; 判断是否均是独立 item
(define (distinct? items)
  (cond
    [(null? items) true]
    [(null? (cdr items)) true]
    [(member (car items) (cdr items)) false]
    [else (distinct? (cdr items))]))

;; 一个比较慢的方法, 简单枚举所有可能性并加上约束
;; 习题 4.39/40 讨论了一些改进
(define (multiple-dwelling)
  (let ([baker (amb 1 2 3 4 5)]
        [cooper (amb 1 2 3 4 5)]
        [fletcher (amb 1 2 3 4 5)]
        [miller (amb 1 2 3 4 5)]
        [smith (amb 1 2 3 4 5)])
    (require (distinct?
              (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    ;; (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)
(amb)
(amb)
(amb)
(amb)

;; 5 个解kk
