#lang sicp

;; 函数体不止可以有一行
(define (for-each proc items)
  (let ([items-cdr (cdr items)])
    (proc (car items))
    (if (not (null? items-cdr))
        (for-each proc items-cdr)
        #t)))

(for-each (lambda (x) (newline)
            (display x))
          (list 57 321 88))

