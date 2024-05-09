#lang sicp

(rule (can-replace ?person-1 ?person-2)
      (and (job ?person-1 ?job-1)
           (job ?person-2 ?job-2)
           (not (same ?person-1 ?person-2))
           (or (same ?job-1 ?job-2)
               (can-do-job ?job-1 ?job-2))))

;; The query output should be propagated after we finished our logic programming language

;; a.
;;; Query input:
(can-replace ?person (Fect Cy D))

;;; Query results:
(can-replace (Hacker Alyssa p) (Fect cy d))
(can-replace (Bitdiddle Ben) (Fect cy d))

;; b.
(and (can-replace ?p1 ?p2)
     (salary ?p1 ?s1)
     (salary ?p2 ?s2)
     (lisp-value > ?s2 ?s1))

;; Output:
(and (salary (Aull DeWitt) 25000)
     (salary (Warbucks Oliver) 150000)
     (replace (Aull DeWitt) (Warbucks Oliver))
     (lisp-value > 150000 25000))
(and (salary (Fect Cy D) 35000)
     (salary (Hacker Alyssa P) 40000)
     (replace (Fect Cy D) (Hacker Alyssa P))
     (lisp-value > 40000 35000))
