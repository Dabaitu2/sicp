#lang sicp


;; suppose we got such form of information of the personnel data
;;
;; Here's a resident computer wizard:
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)


;; and those are his subordinates
(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))


(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit lem e) (Bitdiddle Ben))

;; There's also a programmer trainee, who is supervised by Alyssa:
;;
(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee));;
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))


;; Ben's supervisor is the company's big wheel(大人物，大佬)
(supervisor (Bitdiddle Ben) (Warbucks Oliver))
(address (Warbucks Oliver) (Swellesley (Top Heap Road)))jj
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)


;; Besides the computer division supervised by Ben, the company has an
;; accounting division(部门), consisting of a chief accountant and his assistant:
;;
(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))

;; There is also a secretary for the big wheel:
(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))

;; the data base also contains assertions about which kinds of jobs can be done by people holding other kinds of jobs.
;; For instance, a computer wizard can do the jobs of both a computer programmer and a computer technician:
;;
(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))
(can-do-job (computer programmer)
           (computer programmer trainee))

;;  Also, as is well known ;)
(can-do-job (administration secretary)
           (administration big wheel))

