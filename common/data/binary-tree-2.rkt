#lang sicp

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

;; constructor 前序遍历
(define (make-tree gt? lt? item-eq? entry left right)
  (let ([tree (list entry left right)])
    ;; selectors
    (define (entry)
      (car tree))
    (define (left-branch)
      (cadr tree))
    (define (right-branch)
      (caddr tree))
    (define (lookup given-key)
      (define (lookup-iter given-key set-of-records)
        (cond
          [(null? set-of-records) #f]
          [(item-eq? given-key (entry set-of-records))
           (entry set-of-records)]
          [(gt? given-key (entry set-of-records))
           (lookup-iter given-key
                        (right-branch set-of-records))]
          [else
           (lookup-iter given-key
                        (left-branch set-of-records))]))
      (lookup-iter given-key tree))

    (define (add el)
      (define (adjoin-set x set)
        (cond
          [(null? set) (make-tree x '() '())]
          [(item-eq? x (entry set)) set]
          [(lt? x (entry set))
           (make-tree (entry set)
                      (adjoin-set x (left-branch set))
                      (right-branch set))]
          [(gt? x (entry set))
           (make-tree gt?
                      lt?
                      item-eq?
                      (entry set)
                      (left-branch set)
                      (adjoin-set x (right-branch set)))]))
      (adjoin-set el tree))

    (define (dispatch m)
      (cond
        [(eq? m 'entry) (entry)]
        [(eq? m 'left-branch) (left-branch)]
        [(eq? m 'right-branch) (right-branch)]
        [(eq? m 'lookup) lookup]
        [(eq? m 'add) add]))
    dispatch))

#| (define t (make-tree > < = 1 '() '())) |#
#| (t 'entry) |#
#| (t 'left-branch) |#



;; constructor
(#%provide make-tree)
