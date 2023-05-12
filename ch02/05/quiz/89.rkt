#lang sicp

(define (make-term order coeff)
  (list order coeff))
(define (order term)
  (car term))
(define (coeff term)
  (cadr term))
;; achive dense polynomials

;; 是 coeff 的 list, adjoin-term 隐含了条件: 增加的项 order 一定是最高次数 + 1
;; 因此 term-list 并不是 term 的 list. 我们需要 term 的时候需要把 order 找出来再附带上
;; 注意：在接下来的习题中要求我们兼容这两种表示方法，会变得很恶心
;; 很多 adjoin-term 需要根据放入的是 coeef 还是 term 分开讨论
;; 此外，这里的 term 的最低次数为 0, 不支持负数次数
(define (adjoin-term term term-list)
  (if (emtpy-termlist? term-list)
      (coeff term)
      (list (coeff term) term-list)))

(define (first-term termlist)
  (make-term (- (length termlist)) (car termlist)))

(define (rest-term term-list)
  (cdr term-list))

(define (emtpy-termlist? term-list)
  (null? term-list))

;; 这种做法 虽然并不完全贴合题意,  但就不需要做兼容了，不过我们还是好好做题，采用上面的做法
;; (define (first-term term-list)
;;   (make-term (- (length term-list) 1)
;;              (car term-list)))
;;
;; (define (adjoin-term term term-list)
;;   (let ((coeff-term (coeff term))
;;         (order-term (order term))
;;         (length-terms (length term-list)))
;;     (cond
;;       ((= order-term length-terms) (cons coeff-term term-list))
;;       ((< order-term length-terms) (error "Cannot adjoin lower-order term to terms"))
;;       (else (cons coeff-term (adjoin-term (make-term (- order-term 1) 0) term-list))))))
