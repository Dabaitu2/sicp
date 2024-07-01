#lang sicp

(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))
(meeting whole-company (Wednesday 4pm))

;; a.
(rule (todays-meetings ?weekday)
      (meeting ?division (?weekday ?time)))

;; b
;; querying for all meetings of that person's division
;; plus whole-company's meetings
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time))
      (and (job person? (?division ?job))
           (meeting ?division ?day-and-time)))

;; c
;; should be
(and (meeting-time ? (Hacker Alyssa P) (Wednesday ?time))
     (lisp-value >= 12 ?time))
