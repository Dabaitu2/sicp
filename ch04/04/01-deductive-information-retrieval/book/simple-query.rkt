#lang sicp
;; Simple Queries

;; ;;; Query input:
;; (job ?x (computer programmer)) => grammer 1， ?<VAR>
;;
;; ;;; Query results:
;; (job (Hacker Alyssa P) (computer programmer))
;; (job (Fect Cy D) (computer programmer))



;; ;;; Query input:
;; (address ?x ?y) => grammer 2, multiple variables, 

;; ;;; Query results:  get all suitable single element part
;; (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
;; (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
;; (address (Fect Cy D) (Cambridge (Ames Street) 3))
;; (address (Aull DeWitt) (Slumerville (Park Place) 5))
;; (address (Scrooge Eben) (Weston (Shady Lane) 10))
;; (address (Cratchet Robert) (Allston (N Harvard Street) 16))
;; (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
;; (address (Tweakit Lem E) (Boston (Bay State Road) 22))


;; ;;; Query input: grammer 3, multiple variables with same name
;; (supervisor ?x ?x)

;; ;;; Query results:
;; => generally, it should return all supervised who supervised themselves


;; ;;; Query Input: => grammer 4, ". ?<VAR>" can match any list element rather than single element
;; (computer . ?type);
;;
;; ;;; Query results:
;; (computer programmer trainee)
;; (computer programmer)
;; (computer)


;; ;;; Query Input => grammer 5, query without variable, just try to match the only record or return nothing
;; (job (Bitdiddle Ben) (computer wizard))


;; In conclusion
;; 1. the system find s all assignmments to variables in the query pattern that satisfy the pattern—
;;    that is, all sets of values for the variables such that if the pattern variables are instantiated with (replaced by) the values, the result is in the data base.
;; 2. the system responds to the query by listing all instantiations of the query pattern with the variable assignments that satisfy it.

;; 系统将找出使得查询模式中变量满足这一模式的所有赋值 ，也就是说，为这些变量找出 所有的值集合，使得如果将这些模式变量用这样的一组值实例化 (取代)，得到的结果 就在这 个数据库 里。
;; 系统对查询的响应方式，就是列出查询模式的所有满足要求的实例，这些实例可以通过 将模式中的变量赋为满足它的值而得到
