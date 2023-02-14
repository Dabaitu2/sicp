#lang sicp

;; 依次打印每一个叶子结点
(define (fringe x)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (append result
                      (let ([first (car items)])
                        (if (pair? first)
                            (iter first nil)
                            (list first)))))))
  (iter x nil))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
;; (1 2 3 4)
(fringe (list x x))
;; (1 2 3 4 1 2 3 4)


