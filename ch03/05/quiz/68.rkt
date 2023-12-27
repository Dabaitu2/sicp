#lang sicp

(#%require "../../../common/data/stream.rkt")

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   ;; 这里就交换顺序，让递归的元素变成 s2， 从而让 s1 和 s2 交错
                   (interleave s2 (stream-cdr s1)))))


(define (louis-pairs s t)
  (display "you can see me infinitely")
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x)) t)
   (louis-pairs (stream-cdr s) (stream-cdr t))))


(define rst (louis-pairs integers integers ))

;; louis-pairs cannot work well
;; becasue not like cons-stream, 
;; the procedure interleave will be evaluated 
;; which measns louis-pairs will be invoked recursive and infinitely


;; so we need a cons-stream to stop the evaluation before that



