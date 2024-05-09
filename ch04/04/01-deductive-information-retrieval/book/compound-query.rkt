#lang sicp

;; One thing that makes the query language a logic programming language is that
;; the means of combination mirror the means of combination used in forming logical expressions: and, or, and not.

;; mirror 可以理解为仿造，类比

;; Query input:
(and (job ?person (computer programmer))
     (address (?person where)))

;; Query outpu
(and (job (Hacker Alyssa P) (computer programmer))
     (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))

(and (job (Fect Cy D) (computer programmer))
     (address (Fect Cy D) (Cambridge (Ames Street) 3)))

;; Query Input: Get Ben / Hacker's supervisor
(or (supervisor ?x (Bitdiddle Ben))
    (supervisor ?x (Hacker Alyssa P)))

;; Query Output:
;; 这里可能产出一些没什么道理的东西，因为我们两个变量都用的是 ?x
(or (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
    ;; 比如这个
    (supervisor (Hacker Alyssa P) (Hacker Alyssa P)))
(or (supervisor (Fect Cy D) (Bitdiddle Ben))
    (supervisor (Fect Cy D) (Hacker Alyssa P)))
(or (supervisor (Tweakit Lem E) (Bitdiddle Ben))
    (supervisor (Tweakit Lem E) (Hacker Alyssa P)))
(or (supervisor (Reasoner Louis) (Bitdiddle Ben))
    (supervisor (Reasoner Louis) (Hacker Alyssa P)))

;; Query Input:
(and (supervisor ?x (Bitdiddle Ben))
     (not (job ?x (computer programmer))))

;; Query Output:
(and (supervisor (Tweakit lem e) (Bitdiddle Ben))
     (not (job (Tweakit lem e) (computer programmer))))

;; Query Input:
;; (lisp-value <predicate> <arg1> . . . <argn>)
;; 
;; 搜索到同时满足：
;; 1 是 salary 中的第二个元素 
;; 2 > 30000
;; 的实例
;; find all instance of salary which person and amount is satisfy all the args
(and (salary ?person ?amount) (lisp-value > ?amount 30000))
