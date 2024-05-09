#lang sicp

(rule (?x next-to ?y in (?x ?y . ?u)))
(rule (?x next-to ?y in (?v . ?z)) 
      (?x next-to ?y in ?z))


(?x next-to ?y in (1 (2 3) 4))

;; => (1 next-to (2 3) in (1 (2 3) 4))
;; => NO (2 next-to 3 in (2 3)) because (?x ?y . ?u) can not hit (2 3))
;; => ((2 3) next-to 4 in (1 (2 3) 4))

(?x next-to 1 in (2 1 3 1))

;; => 2 next-to 1 in (2 1 3 1)
;; => NO 1 next-to 3 in (1 3 1), 
;; because (?v . ?z) can not hit (2 .. 1) for 1 has been assigned to ?x 
;; => 3 next-to 1 in (2 1 3 1)
