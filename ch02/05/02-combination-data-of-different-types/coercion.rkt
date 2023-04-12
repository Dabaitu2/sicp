#lang sicp

(define coercion-list '())

(define (clear-coercion-list)
  (set! coercion-list '()))

(define (put-coercion type1 type2 item)
  (if (get-coercion type1 type2)
      coercion-list
      (set! coercion-list
            (cons (list type1 type2 item) coercion-list))))

(define (get-coercion type1 type2)
  (define (get-type1 listItem)
    (car listItem))
  (define (get-type2 listItem)
    (cadr listItem))
  (define (get-item listItem)
    (caddr listItem))
  (define (get-coercion-iter list type1 type2)
    (if (null? list)
        #f
        (let ([top (car list)])
          (if (and (equal? type1 (get-type1 top))
                   (equal? type2 (get-type2 top)))
              (get-item top)
              (get-coercion-iter (cdr list) type1 type2)))))
  (get-coercion-iter coercion-list type1 type2))

(#%provide put-coercion get-coercion)
