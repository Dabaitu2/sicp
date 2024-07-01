#lang sicp

;; Beginning with the database and the rules you formulated in Exercise 4.63,
;; devise a rule for adding “greats” to a grandson relationship.
;;
;; This should enable the system to deduce that
;; Irad is the great-grandson of Adam,
;; or that Jabal and Jubal are the great-great-great-great-great-grandsons of Adam.

;; Hint:
;; Represent the fact about Irad, for example, as
;; ((great grandson) Adam Irad).
;; Write rules that determine if a list ends in the word grandson.
;; Use this to express a rule that allows one to derive the relationship
;; ((great . ?rel) ?x ?y), where ?rel is a list ending in grandson.

;; Check your rules on queries such as ((great grandson) ?g ?ggs) and (?relationship Adam Irad).

;; 设计出一个规则推断出 "重" 关系
;; 比如 ((great grandson) ?a ?b) 标识 ?b 是 ?a 的重孙

(rule (find-son ?F ?S)
      (or (son ?F ?S)
          (and
           (wife ?F ?W)
           (son ?W ?S))))
(rule (grandson ?G ?S) (and (son ?F ?S) (son ?G ?F)))

(rule (end-with-grandson) (grandson))
(rule (end-with-grandson (?prefix . ?rel)
                         (end-with-grandson ?rel)))

;; 一定要记得 rule 里的变量是可以在 query input 的时候填充的，所以通常的用法是 ((great grandson) ?x ?y)
;; 最妙的地方便是传进去的参数也可以作为规则 conclusion 本身被调用
(rule ((great . ?rel) ?x ?y)
      (and
        ;; ?rel 必须是 grandson 结尾
       (end-with-grandson ?rel)
       ;; 找到 ?x 的儿 ?z
       (find-son ?x ?z)
       ;; 找到符合 ?rel 规则下的 ?z ?y
       ;; ?rel 可能是递归的 (great . ?subrel) 也可能是最终的 (son ?a ?b)
       (?rel ?z ?y)
       ))
