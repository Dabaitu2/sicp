#lang sicp
(#%require "../../../common/data/queue.rkt")

;; Achieve Agenda
;; Agenda is a magic timeline, it just like god's schedule list, prophet the time of specific things' happening

;; Agenda constructs with time-segment
;; Each time segment is a pair consiting of a number(the time) and a queue(store all procedures that should be run during that segment)
(define (make-time-segment time queue)
  (cons time queue))
(define (segement-time s)
  (car s))
(define (segement-queue s)
  (cdr s))

;; agenda is (list <current-time> <table of time-segement with sequence based on time of the segement>)
;; constructor
(define (make-agenda)
  ;; 0 is initial current-time
  (list 0))

;; selectors
(define (current-time agenda)
  (car agenda))
(define (segements agenda)
  (cdr agenda))
(define (first-segement agenda)
  (car (segements agenda)))
(define (rest-segements agenda)
  (cdr (segements agenda)))

;; mutators
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (set-segements! agenda segements)
  (set-cdr! agenda segements))

;; predicates
(define (empty-agenda? agenda)
  (null? (segements agenda)))

(define (add-to-agenda! time action agenda)
  ;; 检查要注册的 time 是否在 time_segement 的前面
  (define (belongs-before? segements)
    (or (null? segements)
        (< time
           (segement-time (car segements)))))

  ;; 为传入的 action, time 创建一个新的 time-segment
  (define (make-new-time-segement time action)
    (let ([q (make-queue)])
      ((q 'insert-queue!) action)
      (make-time-segment time q)))

  ;; 将 action 插入 segements 的适当位置中
  (define (add-to-segements! segements)
    (if (= (segement-time (car segements)) time)
        (((segement-queue (car segements)) 'insert-queue!)
         action)
        (let ([rest (cdr segements)])
          (if (belongs-before? rest)
              (set-cdr!
               segements
               (cons (make-new-time-segement time action)
                     (cdr segements)))
              (add-to-segements! rest)))))

  ;; 实际逻辑
  (let ([segements (segements agenda)])
    ;; 如果时间在所有 time-segment 最前面
    (if (belongs-before? segements)
        ;; 就将新 segements 插入 segements 最前面
        (set-segements!
         agenda
         (cons (make-new-time-segement time action)
               segements))
        ;; 否则执行内部插入操作
        (add-to-segements! segements))))

;; api based on basic selectors
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ([first-seg (first-segement agenda)])
        ;; here's effect
        ;; we do this because we want even the segement has been removed, the `current-time`
        (set-current-time! agenda (segement-time first-seg))
        ((segement-queue first-seg) 'front-queue))))

;; api based on basic mutators
(define (remove-first-agenda-item! agenda)
  (let ([q (segement-queue (first-segement agenda))])
    (q 'delete-queue!)
    (if (q 'empty-queue?)
        (set-segements! agenda (rest-segements agenda))
        ;; else 可以不写
        )))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'propagate-done
      (let ([first-item (first-agenda-item the-agenda)])
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define the-agenda (make-agenda))
(#%provide make-agenda
           empty-agenda?
           first-agenda-item
           add-to-agenda!
           remove-first-agenda-item!
           current-time
           after-delay
           propagate
           the-agenda)
