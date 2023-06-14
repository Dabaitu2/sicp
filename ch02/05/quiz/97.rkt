#lang racket

;; 结合求 gcd 的操作将有理函数化简为最简形式

;; a
(define (reduce-terms n d)
  (let ((gcd-n-d (gcd-temrs n d)))
    (list (car (div-terms n gcd-n-d))
          (car (div-terms d gcd-n-d)))
    )
  )


(define (reduce-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (map (lambda (x) (make-poly (variable p1) x))
           (reduce-terms (term-list p1) (term-list p2)))
      (let ([priority-order (order-by-priority p1 p2)])
        (let ([target-var
               (variable (car priority-order))])
          (reduce-poly (make-canonical target-var p1)
                       (make-canonical target-var p2))))))

(put 'reduce-poly
     '(polynomial polynomial)
     (lambda (p1 p2)
       (map (lambda (x) (tag x)) (reduce-poly p1 p2))))
