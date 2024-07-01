#lang sicp

;; suppose we got such form of information of the personnel data
;;
;; Here's a resident computer wizard:
(assert! (address (Bitdiddle Ben)
                  (Slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))

;; and those are his subordinates
(assert! (address (Hacker Alyssa P)
                  (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))

(assert! (address (Tweakit Lem E)
                  (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit lem e) (Bitdiddle Ben)))

;; There's also a programmer trainee, who is supervised by Alyssa:
;;
(assert! (address (Reasoner Louis)
                  (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis)
              (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))

;; Ben's supervisor is the company's big wheel(大人物，大佬)
(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(assert! (address (Warbucks Oliver)
                  (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))

;; Besides the computer division supervised by Ben, the company has an
;; accounting division(部门), consisting of a chief accountant and his assistant:
;;
(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))
(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

;; There is also a secretary for the big wheel:
(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

;; the data base also contains assertions about which kinds of jobs can be done by people holding other kinds of jobs.
;; For instance, a computer wizard can do the jobs of both a computer programmer and a computer technician:
;;
(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))
(assert! (can-do-job (computer programmer)
                     (computer programmer trainee)))

;;  Also, as is well known ;)
(assert! (can-do-job (administration secretary)
                     (administration big wheel)))
