#lang sicp
(#%require "../../../common/algo/permutation.rkt")
(#%require
 "../../../common/data/conventional-interface.rkt")

;; 写出一个常规的 scheme 程序以解决此问题 (multiple-dwelling)

;; 最容易懂的办法：全排列检查 permutations
(define (check lst)
  (define baker (car lst))
  (define cooper (cadr lst))
  (define miller (caddr lst))
  (define fletcher (cadddr lst))
  (define smith (car (cddddr lst)))
  (and (not (= baker 5))
       (not (= cooper 1))
       (not (= fletcher 5))
       (not (= fletcher 1))
       (> miller cooper)
       (not (= (abs (- smith fletcher)) 1))
       (not (= (abs (- fletcher cooper)) 1))))

(define stime (runtime))
(filter check (permutations (list '1 2 3 4 5)))
(display (list "Time Taken: " (- (runtime) stime)))
(newline)


;; 回溯法，类似 N 皇后
;; 一个一个检查直到最后一个, 不过和八皇后不同在于我们需要比较确定的去安排这里检测的关系
;; 因为要保证前面检测的不依赖后面才会安排依赖的人
(define baker 0)
(define cooper 1)
(define miller 2)
(define fletcher 3)
(define smith 4)

(define (safe? guys-floors)
  (define (check who floors)
    ;; (display "floors: ")
    ;; (display floors)
    ;; (display ", who: ")
    ;; (display who)
    ;; (newline)
    (if (> who (- (length floors) 1))
        #t
        (let ([cur-floor (list-ref floors who)])
          (if (cond
                [(= who baker) (not (= cur-floor 5))]
                [(= who cooper) (not (= cur-floor 1))]
                [(= who miller)
                 (let ([cooper-floor (list-ref floors
                                               cooper)])
                   (> cur-floor cooper-floor))]
                [(= who fletcher)
                 (let ([cooper-floor (list-ref floors
                                               cooper)])
                   (and (not (= cur-floor 1))
                        (not (= cur-floor 5))
                        (not (= (abs (- cur-floor
                                        cooper-floor))
                                1))))]
                [(= who smith)
                 (let ([fletcher-floor (list-ref floors
                                                 fletcher)])
                   (and (not (= (abs (- cur-floor
                                        fletcher-floor))
                                1))
                        (distinct? floors)))]
                [else (error "should not go there")])
              (check (+ who 1) floors)
              #f))))
  (check 0 guys-floors))

(define (dwells man-size)
  (define (dwell-guys-floors k)
    (if (= k 0)
        (list '())
        ;; 检查当前的 floor 配置是否满足条件
        (filter (lambda (guys-floors) (safe? guys-floors))
                ;; 对已经确认完毕的 前几个人-楼层
                ;; 将新人从 1 ~ 5 楼的可能性依次和前面的人所构成的 floor 链接
                ;; 然后 flatmap 链接到一起
                ;; 作为待检查的对象
                (flatmap
                 (lambda (arranged-guys-floors)
                   (map (lambda (new-floor)
                          (append arranged-guys-floors
                                  (list new-floor)))
                        (enumerate-interval 1 5)))
                 ;; 余下已经确认完毕的人构成的集合
                 (dwell-guys-floors (- k 1))))))
  (dwell-guys-floors man-size))

(define stime2 (runtime))
(dwells 5)
(display (list "Time Taken: " (- (runtime) stime2)))
(newline)
