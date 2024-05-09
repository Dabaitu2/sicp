#lang sicp

;; suppose we have a query rule that finds the people who live near a given person
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?place . ?rest-1))
           (address ?person-2 (?place . ?rest-2))
           (not (same ?person-1 ?person-2))))

;; generally it will return the lists with swapped order twice
(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))

;; because the (not (same )) rule didn't check the order of the arguments
;; so person-1 & person-2 which occur in different order

;; if we wanna just return one of the order, we can add a restriction to confine the order
;; make Person record => string
(define (person->string person)
  (if (null? person)
      ""
      (string-append (symbol->string (car person))
                     (person->string (cdr person)))))

;; check the order of person-1 & person-2 by compare string order
(define (person>? p1 p2)
  (string>? (person->sring p1) (person->string p2)))

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?place . ?rest-1))
           (address ?person-2 (?place . ?rest-2))
           (not (same ?person-1 ?person-2))
           (lisp-value person>? ?person-1 ?person-2)))
