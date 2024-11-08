#lang sicp

;; 针对 factorial 的迭代过程 的控制流
(controller
  (assign product (const 1))
  (assign counter (const 1))
 iter
  (test (op >) (reg product) (reg counter))
  (branch (label iter-done))
  (assign product (op *) (reg counter) (reg product))
  (assign counter (op +) (reg counter) (const 1))
  (goto (label iter))
 iter-done)


