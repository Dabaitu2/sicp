#lang sicp

;; helpers
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))
(define (square-tree tree)
  (cond
    [(null? tree) nil]
    [(not (pair? tree)) (* tree tree)]
    [else (cons (square-tree (car tree))
                (square-tree (cdr tree)))]))

(define (square-tree-2 tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree-2 subtree)
             (* subtree subtree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
(square-tree-2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
