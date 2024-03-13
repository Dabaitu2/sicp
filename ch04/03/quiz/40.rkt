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

;; 将所有的数据在一开始产出，再回溯去重新生成是非常低效的，因为
;; 大部分的约束条件都只依赖于 one or two person-floor variables
;; 因此可以在为所有人选择楼层之前安排好，尽可能减少无效回溯
;; 例如下面的例子，fletcher，cooper， smith，miller 之前存在关系，这意味着 backer 不重要
;; 完全可以放到最后. 只需要 26ms !
(define (multiple-dwelling)
  (let ([cooper (amb 2 3 4 5)] [fletcher (amb 2 3 4)])
    (require (not (= (abs (- fletcher cooper)) 1)))
    (let ([smith (amb 1 2 3 4 5)])
      (require (not (= (abs (- smith fletcher)) 1)))
      (let ([miller (amb 1 2 3 4 5)])
        (require (> miller cooper))
        (let ([baker (amb 1 2 3 4)])
          (require (distinct? (list baker
                                    cooper
                                    fletcher
                                    miller
                                    smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

;; 仅有 1 个解
(define stime (runtime))
(multiple-dwelling)
(display (list "Time Taken: " (- (runtime) stime)))
