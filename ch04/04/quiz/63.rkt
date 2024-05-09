#lang sicp

(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)

;; Formulate rules such as
;; “If S is the son of f, and f is the son of G,then S is the grandson of G”
;; and
;; “If W is the wife of M,and S is the son of W,then S is the son of M”
(rule (find-son ?M ?S)
      (or (son ?M ?S) 
          (and 
            (wife ?M ?W)
            (son ?W ?S))))

(rule (grandson S? G?) (and (son S? F?) (son F? G?)))
