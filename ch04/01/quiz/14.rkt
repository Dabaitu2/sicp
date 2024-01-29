#lang sicp

;; the reason why internal map failed
;; is => 

;; intenal map, as a primitive-procedure accept a lambda, but it should be the lambda defined by racket
;; rather than our evaluator, out lambda, in other word will be evaluated to 
;; a make-procedure and become `(list 'procedure params body env)` 
;; which is actually a list rather than a procedure
