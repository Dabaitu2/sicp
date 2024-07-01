#lang sicp

(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

;; Formulate rules such as
;; “If S is the son of f, and f is the son of G,then S is the grandson of G”
;; and
;; “If W is the wife of F,and S is the son of W,then S is the son of F”
(assert! (rule (find-son ?F ?S)
               (or (son ?F ?S)
                   (and
                    (wife ?F ?W)
                    (son ?W ?S)))))

(assert! (rule (grandson G? S?) (and (son F? S?) (son G? F?))))
