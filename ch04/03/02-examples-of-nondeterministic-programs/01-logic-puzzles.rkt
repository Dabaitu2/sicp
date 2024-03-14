#lang sicp

;; 在这里我们会利用几个实例来展示非确定性计算的威力
;; 在不需要实现具体计算细节的基础上就可以实现对问题的求解

;; 1. Logic Puzzles 逻辑谜题
;; Baker, Cooper, Fletcher, Miller, and Smith
;; live on different floors of an apartment house that contains only five floors.
;;
;; Baker does not live on the top floor.
;; Cooper does not live on the bottom floor.
;; Fletcher does not live on either the top or the bottom floor.
;; Miller lives on a higher floor than does Cooper.
;; Smith does not live on a floor adja- cent to Fletcher’s.
;; Fletcher does not live on a floor adjacent to Cooper’s.
;;
;; Where does everyone live?

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
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; 仅有 1 个解
(define stime (runtime))
(multiple-dwelling)
(display (list "Time Taken: " (- (runtime) stime)))

