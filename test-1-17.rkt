#lang sicp
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
(* 3 4)