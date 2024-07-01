#lang sicp

;; Rule is a means of abstract querying

(assert! (rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2)))))

;; a rule without body means any values of the variable can satisfy the pattern
;; 实际上，如果要判断两个东西一样，是不需要专门写一条 rule 的，
;; 因为只要我们的查询写上同样的变量名本来就暗含了这样的搜索条件
;;  所以这里的 rule 本质上没有意义 它只有和 not 结合的时候才有意义
;; 本质上，就是先匹配任何传进来的东西，然后用 not 去把传进来的 ?x 去掉
(assert! (rule (same ?x ?x)))
(assert! (rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager))))

;; Query Inputs:
(lives-near ?x (Bitdiddle Ben))

;; Query Output:
(lives-near (Reasoner Louis) (Bitdiddle Ben))
(lives-near (Aull DeWitt) (Bitdiddle Ben))

;; Query Inputs for find person who live near Ben while work in computer division:
(and (job ?x (computer programmer))
     (lives-near ?x (Bitdiddle Ben)))

;; Query Output:
;; ...

;; outrank: 超越, 地位比xx高
;; 要么是直接下级，要么是间接下级，这里也可以递归检测
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))
