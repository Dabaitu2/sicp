#lang sicp


;; 71
;; max = (n-1)  while frequency is the least (root node will not occupy any bit position)
;; min = 1 while frequency is the max
;;
;;                     {a b c d e} 31
;;                     /           \
;;                {a b c d} 15      e 16
;;                 /     \
;;           {a b c} 7    d 8
;;             /    \
;;        {a b} 3    c 4
;;         /   \
;;      a 1    b 2



;; 72
;; considering the specific situation described in 71
;; for most frequent occuring bit it's O(n): we need to check symbols once, which will iterate the whole left/right sub-tree
;; for least frequent occuring bit it's O(n * n-1) ∈ O(n^2): we need to check symbols n-1 times
