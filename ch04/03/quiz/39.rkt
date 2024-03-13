#lang sicp


;; 1. 不会影响答案
;; 2. 然而，顺序会影响找到答案的时间
;; 因为，distinct 是比较耗时的操作，然而一旦它放在最前面被使用
;; 则后面任何一个严格条件失败，这个 distinct 过程都得被重新算一次

;; 而我们完全可以将这个操作放到最后面，这样前面的常数项时间判断结束之后
;; 即使判断不过关，产出新数据之后常数项的再次判断也不会消耗太多时间

;; 3. 一种可能更优的排布
;; 可以看到时间缩短到 185 左右
;; 而正文给出的方法在 260 左右

(define (require p)
  (if (not p) (amb)))

;; 判断是否均是独立 item
(define (distinct? items)
  (cond
    [(null? items) true]
    [(null? (cdr items)) true]
    [(member (car items) (cdr items)) false]
    [else (distinct? (cdr items))]))

(define (multiple-dwelling)
  (let ([baker (amb 1 2 3 4 5)]
        [cooper (amb 1 2 3 4 5)]
        [fletcher (amb 1 2 3 4 5)]
        [miller (amb 1 2 3 4 5)]
        [smith (amb 1 2 3 4 5)])
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (not (= (abs (- smith fletcher)) 1)))
    ;; 把宽松判断下移
    (require (> miller cooper))
    (require (distinct?
              (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; 仅有 1 个解
(define stime (runtime))
(multiple-dwelling)
(display (list "Time Taken: " (- (runtime) stime)))










